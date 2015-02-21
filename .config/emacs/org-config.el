(provide 'org-config)


(defun org-mode-settings ()
  (require 'org-install)
  (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
  (global-font-lock-mode 1)
  (setq org-log-done t)
  (setq org-agenda-files (list "~/doc/task/new.org")))

;; -------------------- hooks --------------------

(defun org-config-packages ()
  '())

(defun org-config-on-load ()
  )

(defun org-config-on-configure ()
  (org-mode-settings))

(defun org-config-on-exit ()
  )
