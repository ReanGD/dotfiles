(provide 'sys/packages)

(require 'cl)

(require 'sys/groups)
(require 'sys/hooks)
(require 'sys/vars)
(require 'sys/funcs)

(defun lcl:string-require (name)
  (setq full-name (concat "(require '" name ")"))
  (setq func (car (read-from-string full-name)))
  (eval func))

(defun lcl:one-lvl (ls)
  (if ls (append (car ls) (lcl:one-lvl (cdr ls)))))

(defun lcl:string-call (base postfix)
  (setq full-name (concat base "-" postfix))
  (setq func (car (read-from-string full-name)))
  (if (functionp func)
      (funcall func)
    (message "WARNING: can't find function '%s'" full-name)))

(defun lcl:fill-post-func (v)
  (if (not (listp v)) (list v nil) v))

(defun lcl:get-all-packages (name-list)
  (setq raw-list (mapcar (lambda (name) (lcl:string-call name "packages")) name-list))
  (mapcar 'lcl:fill-post-func
	  (lcl:one-lvl raw-list)))

(defun lcl:need-install-package (package-list)
  (loop for p in package-list
        when (not (package-installed-p (car p)))
	do (return t)
        finally (return nil)))

(defun lcl:install-packages (name-list)
  (setq all-packages (lcl:get-all-packages name-list))
  (when (lcl:need-install-package all-packages)
    (message "%s" "Emacs is now refreshing its package database...")
    (package-refresh-contents)
    (message "%s" " done.")
    (dolist (p all-packages)
      (when (not (package-installed-p (car p)))
	(package-install (car p))
	(setq func (car (cdr p)))
	(if func (funcall func)))))
  )

(defun lcl:init-dir ()
  (setq delete-dir nil)

  (if (eq cfg-var:work-dir nil)
      (setq cfg-var:work-dir user-emacs-directory)
    (setq delete-dir user-emacs-directory))
  
  (setq backup-dir (concat (file-name-as-directory cfg-var:work-dir) "backup")
	auto-save-dir (concat (file-name-as-directory cfg-var:work-dir) "auto-save"))

  (setq user-emacs-directory cfg-var:work-dir
	backup-directory-alist `(("." . ,backup-dir))
	auto-save-list-file-prefix auto-save-dir)

  (if (not (eq delete-dir nil))
      (delete-directory delete-dir t)))

(defun cfg:init (name-list)
  (lcl:init-dir)
  (require 'package)
  (add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/") t)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
  (package-initialize)
  (mapcar 'lcl:string-require name-list)
  (mapcar (lambda (name) (lcl:string-call name "pre-load")) name-list)
  (lcl:install-packages name-list)
  (run-hooks 'cfg-hook:ui)
  (run-hooks 'cfg-hook:minor-mode)
  (run-hooks 'cfg-hook:major-mode)
  (run-hooks 'cfg-hook:session)
  (run-hooks 'cfg-hook:hotkey)
  (mapcar (lambda (name) (lcl:string-call name "load")) name-list)
  (mapcar (lambda (name) (lcl:string-call name "post-load")) name-list))
