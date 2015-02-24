(provide 'sys/packages)

(require 'cl)
(require 'package)

(require 'sys/hooks)

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

(defun cfg:init (name-list)
  (add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/") t)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
  (package-initialize)
  (mapcar 'lcl:string-require name-list)
  (mapcar (lambda (name) (lcl:string-call name "pre-load")) name-list)
  (lcl:install-packages name-list)
  (mapcar (lambda (name) (lcl:string-call name "load")) name-list)
  (mapcar (lambda (name) (lcl:string-call name "post-load")) name-list))