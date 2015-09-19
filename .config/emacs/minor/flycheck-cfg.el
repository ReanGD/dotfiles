(provide 'minor/flycheck-cfg)


(defun cfg:flycheck ()
  (require 'flycheck)
  (flycheck-define-checker cargo-rust
    "cargo-rust"
    :command ("cargo" "rustc" "--" "-Z" "no-trans")
    :error-patterns
    ((error line-start (file-name) ":" line ":" column ": "
            (one-or-more digit) ":" (one-or-more digit) " error: "
            (or
             ;; Multiline errors
             (and (message (minimal-match (one-or-more anything)))
                  " [" (id "E" (one-or-more digit)) "]")
             (message))
            line-end)
     (warning line-start (file-name) ":" line ":" column ": "
              (one-or-more digit) ":" (one-or-more digit) " warning: "
              (message) line-end)
     (info line-start (file-name) ":" line ":" column ": "
           (one-or-more digit) ":" (one-or-more digit) " " (or "note" "help") ": "
           (message) line-end))
    :modes rust-mode)
  (add-to-list 'flycheck-checkers 'cargo-rust)
  (add-hook 'rust-mode-hook 'flycheck-mode)
)
;; (add-hook 'cfg-hook:minor-mode 'cfg:flycheck)

(cfg:add-package 'flycheck)
