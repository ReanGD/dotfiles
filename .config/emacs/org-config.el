(provide 'org-config)


(defun cfg:org-mode ()
  (require 'org-install)
  ;; native color scheme for literate programming
  (setq org-src-fontify-natively 't)
  (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
  (global-font-lock-mode 1)
  (setq org-log-done t)
  (setq org-agenda-files (list "~/doc/task/new.org")))
(add-hook 'cfg-hook:major-mode 'cfg:org-mode)
