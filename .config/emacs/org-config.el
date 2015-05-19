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

  (defun org-html-paragraph (paragraph contents info)
    "Transcode a PARAGRAPH element from Org to HTML.
CONTENTS is the contents of the paragraph, as a string.  INFO is
the plist used as a communication channel."
    (let* ((parent (org-export-get-parent paragraph))
           (parent-type (org-element-type parent))
           (style '((footnote-definition " class=\"footpara\"")))
           (extra (or (cadr (assoc parent-type style)) "")))
      (cond
       ((and (eq (org-element-type parent) 'item)
             (= (org-element-property :begin paragraph)
                (org-element-property :contents-begin parent)))
        ;; Leading paragraph in a list item have no tags.
        contents)
       ((org-html-standalone-image-p paragraph info)
        ;; Standalone image.
        (let ((caption
               (let ((raw (org-export-data
                           (org-export-get-caption paragraph) info))
                     (org-html-standalone-image-predicate
                      'org-html--has-caption-p))
                 (if (not (org-string-nw-p raw)) raw
                   (concat
                    "<span class=\"figure-number\">"
                    (format (org-html--translate "Figure %d:" info)
                            (org-export-get-ordinal
                             (org-element-map paragraph 'link
                               'identity info t)
                             info nil 'org-html-standalone-image-p))
                    "</span> " raw))))
              (label (org-element-property :name paragraph)))
          (org-html--wrap-image contents info caption label)))
       ;; Regular paragraph.
       (t
        (format "<br /><div%s>\n%s</div>" extra contents)
        ;; (format "%s%s" extra contents)
          ))))
  )

(add-hook 'cfg-hook:major-mode 'cfg:org-mode)

;; (global-set-key "\C-cl" 'org-store-link)
;; (global-set-key "\C-ca" 'org-agenda)
;; (global-set-key "\C-cb" 'org-iswitchb)
