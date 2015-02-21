(provide 'lib-package)

(require 'cl)
(require 'package)

(defun lib-package--string-require (name)
  (setq full-name (concat "(require '" name ")"))
  (setq func (car (read-from-string full-name)))
  (eval func))

(defun lib-package--one-lvl (ls)
  (if ls (append (car ls) (lib-package--one-lvl (cdr ls)))))

(defun lib-package--string-call (base postfix)
  (setq full-name (concat base "-" postfix))
  (setq func (car (read-from-string full-name)))
  (if (functionp func)
      (funcall func)
    (message "WARNING: can't find function '%s'" full-name)))

(defun lib-package--fill-post-func (v)
  (if (not (listp v)) (list v nil) v))

(defun lib-package--get-all-packages (name-list)
  (setq raw-list (mapcar (lambda (name) (lib-package--string-call name "packages")) name-list))
  (mapcar 'lib-package--fill-post-func
	  (lib-package--one-lvl raw-list)))

(defun lib-package--need-install-package (package-list)
  (loop for p in package-list
        when (not (package-installed-p (car p)))
	do (return t)
        finally (return nil)))

(defun lib-package--install-packages (name-list)
  (setq all-packages (lib-package--get-all-packages name-list))
  (when (lib-package--need-install-package all-packages)
    (message "%s" "Emacs is now refreshing its package database...")
    (package-refresh-contents)
    (message "%s" " done.")
    (dolist (p all-packages)
      (when (not (package-installed-p (car p)))
	(package-install (car p))
	(setq func (car (cdr p)))
	(if func (funcall func)))))
  )

(defun lib-package-init (name-list)
  (package-initialize)
  (print name-list)
  (add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/") t)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
  (mapcar 'lib-package--string-require name-list)
  (mapcar (lambda (name) (lib-package--string-call name "pre-load")) name-list)
  (lib-package--install-packages name-list)
  (mapcar (lambda (name) (lib-package--string-call name "load")) name-list)
  (mapcar (lambda (name) (lib-package--string-call name "post-load")) name-list))
