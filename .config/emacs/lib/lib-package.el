(provide 'lib-package)


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

(defun lib-package--get-all-packages (name-list)
  (lib-package--one-lvl (mapcar 'lib-package--get-packages name-list)))



