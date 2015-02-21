(provide 'lib-package)

(require 'cl)
(require 'package)

(defun lib-package--string-require (name)
  (setq full-name (concat "(require '" name ")"))
  (setq func (car (read-from-string full-name)))
  (eval func))

(defun lib-package--string-list-require (name-list)
  (mapcar 'lib-package--string-require name-list))

(defun lib-package--one-lvl (ls)
  (if ls (append (car ls) (lib-package--one-lvl (cdr ls)))))

(defun lib-package--string-call (base postfix)
  (setq full-name (concat base "-" postfix))
  (setq func (car (read-from-string full-name)))
  (if (functionp func)
      (funcall func)
    (message "WARNING: can't find function '%s'" full-name)))

(defun lib-package--get-packages (name)
  (lib-package--string-call name "packages"))

(defun lib-package--fill-post-func (v)
  (if (not (listp v)) (list v nil) v))

(defun lib-package--get-all-packages (name-list)
  (mapcar 'lib-package--fill-post-func
	  (lib-package--one-lvl (mapcar 'lib-package--get-packages
					name-list))))

(defun lib-package--need-install-package (package-list)
  (loop for p in package-list
        when (not (package-installed-p (car p)))
	do (return t)
        finally (return nil)))

(defun lib-package--initialize ()
  (package-initialize)
  (add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/") t)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t))

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

(defun lib-package--on-load (name)
  (lib-package--string-call name "on-load"))

(defun lib-package--on-load-list (name-list)
  (mapcar 'lib-package--on-load name-list))

(defun lib-package--on-configure (name)
  (lib-package--string-call name "on-configure"))

(defun lib-package--on-configure-list (name-list)
  (mapcar 'lib-package--on-configure name-list))

(defun lib-package--on-exit (name)
  (lib-package--string-call name "on-exit"))

(defun lib-package--on-exit-list (name-list)
  (mapcar 'lib-package--on-exit name-list))



