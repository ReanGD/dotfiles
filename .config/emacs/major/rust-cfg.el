(provide 'major/rust-cfg)

(defun lcl:rust-compile-hook ()
  (require 'compile)
  (set (make-local-variable 'compile-command)
       (if (locate-dominating-file (buffer-file-name) "Cargo.toml")
           "cargo run"
         (format "rustc %s && %s" (buffer-file-name)
                 (file-name-sans-extension (buffer-file-name))))))

(defun cfg:rust ()
  (autoload 'rust-mode "rust-mode" nil t)
  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))

  (add-hook 'rust-mode-hook
            (lambda ()
              (setq indent-tabs-mode nil)
              (setq tab-width 4)
              (setq rust-indent-offset 4)
              (racer-activate)
              ))

  (setq racer-rust-src-path "~/.local/share/rust_src/src")
  (setq racer-cmd "/usr/bin/racer")
  (eval-after-load "rust-mode" '(require 'racer))
  (add-hook 'rust-mode-hook 'lcl:rust-compile-hook))
(add-hook 'cfg-hook:major-mode 'cfg:rust)

(cfg:add-package 'rust-mode)
