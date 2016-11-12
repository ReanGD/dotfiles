(provide 'major/org-cfg)
; (provide 'ox-html)

; (defun cfg:org-mode-blog-html-link (link desc info)
;   (let ((base-link (org-html-link link desc info)))
;     (concat "<a target=\"_blank\"" (substring base-link 2))))

; (defun cfg:org-mode-blog-html-final-output (text back-end info)
;   (format "<div dir=\"ltr\" style=\"text-align: left;\" trbidi=\"on\">\n%s\n</div>" text))

; (defun cfg:org-mode-blog-html ()
;   ; (org-export-define-derived-backend 'blog-html 'html
;   ;   :translate-alist '((link . cfg:org-mode-blog-html-link))
;   ;   :filters-alist '((:filter-final-output . cfg:org-mode-blog-html-final-output))
;   ;   )
;   )

; (defun cfg:org-mode-export-blog-html ()
;   (interactive)
;   (org-export-to-buffer 'blog-html "blog-html-export-buffer" nil nil nil t))

(defun cfg:org-mode ()
  ; (cfg:org-mode-blog-html)
  ;; (require 'org-install)
  (add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\)$" . org-mode))
  (require 'org)
  (setq org-export-with-sub-superscripts nil)
  (setq org-html-htmlize-output-type 'inline-css)
  ;; native color scheme for literate programming
  (setq org-src-fontify-natively 't)
  (global-font-lock-mode 1)
  (setq org-log-done t)
  (global-set-key "\C-ca" 'org-agenda)
  (setq org-agenda-custom-commands
        '(("е" "List of all TODO entries"
           ((call-interactively 'org-todo-list)))))
  (setq org-agenda-files (quote ("~/doc/task"))))
(add-hook 'cfg-hook:major-mode 'cfg:org-mode)

(cfg:add-package 'htmlize)
