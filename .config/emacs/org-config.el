(provide 'org-config)


(defun cfg:org-mode ()
  ;; (require 'org-install)
  (add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\)$" . org-mode))
  (require 'org)
  ;; native color scheme for literate programming
  (setq org-src-fontify-natively 't)
  (global-font-lock-mode 1)
  (setq org-log-done t)
  (setq org-agenda-files (quote ("~/doc/task")))
  )

(add-hook 'cfg-hook:major-mode 'cfg:org-mode)

;; (global-set-key "\C-cl" 'org-store-link)
;; (global-set-key "\C-ca" 'org-agenda)
;; (global-set-key "\C-cb" 'org-iswitchb)
