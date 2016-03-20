;;; rust-cfg.el --- Configure rust
;;; Commentary:
;;; Code:
(require 'sys/packages)
(require 'rust-mode nil t)
(require 'company nil t)
(require 'eldoc nil t)
(require 'racer nil t)
(require 'flycheck nil t)
;; (require 'yasnippet nil t)

(defun cfg:rust-flycheck ()
  "Configure rust flycheck."
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
  )

(defun cfg:rust ()
  "Configure rust."
  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))

  (add-hook 'rust-mode-hook
            (lambda ()
              (setq indent-tabs-mode nil  ;; отступы делаются пробелами
                    tab-width 4           ;; default tab width
                    rust-indent-offset 4  ;; rust specific tab width
                    )))

  (setq racer-rust-src-path "/usr/src/rust/src")
  (setq racer-cmd "/usr/bin/racer")

  (cfg:rust-flycheck)
  (add-hook 'rust-mode-hook 'flycheck-mode)
  (add-hook 'rust-mode-hook 'racer-mode)
  (add-hook 'racer-mode-hook 'eldoc-mode)
  (add-hook 'racer-mode-hook 'company-mode)
  )
(add-hook 'cfg-hook:major-mode 'cfg:rust)

(cfg:add-package 'rust-mode)
(cfg:add-package 'racer)
;; (cfg:add-package 'rustfmt)


(provide 'major/rust-cfg)
;;; rust-cfg.el ends here
