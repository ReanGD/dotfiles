(provide 'major/org-cfg)

(defun cfg:org-mode ()
  ;; (require 'org-install)
  (add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\)$" . org-mode))
  (require 'org)
  (setq org-export-with-sub-superscripts nil)
  (setq org-html-htmlize-output-type 'inline-css)
  ;; native color scheme for literate programming
  (setq org-src-fontify-natively 't)
  (global-font-lock-mode 1)
  (setq org-log-done t)
  (setq org-agenda-files (quote ("~/doc/task"))))
(add-hook 'cfg-hook:major-mode 'cfg:org-mode)

(cfg:add-package 'htmlize)
