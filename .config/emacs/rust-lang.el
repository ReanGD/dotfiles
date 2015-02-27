(provide 'rust-lang)


;; http://emacs.stackexchange.com/questions/2137/press-f5-to-save-compile-and-run-current-rust-file
(defun lcl:rust-save-compile-and-run ()
  (interactive)
  (save-buffer)
  (if (locate-dominating-file (buffer-file-name) "Cargo.toml")
    (compile "cargo run")
    (compile
      (format "rustc %s & %s"
        (buffer-file-name)
        (file-name-sans-extension (buffer-file-name))))))

(defun cfg:rust ()
  (autoload 'rust-mode "rust-mode" nil t)
  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
  (setq racer-rust-src-path "/home/rean/tmp/rust/src")
  (setq racer-cmd "/usr/bin/racer")
  ;; (add-to-list 'load-path "<path-to-racer>/editors")
  (eval-after-load "rust-mode" '(require 'racer))
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
  (add-hook 'rust-mode-hook
	    (lambda ()
	      (define-key rust-mode-map (kbd "<f5>") 'lcl:rust-save-compile-and-run))))
(add-hook 'cfg-hook:major-mode 'cfg:rust)

;; -------------------- hooks --------------------

(defun rust-lang-packages ()
  '(rust-mode flycheck-rust company))
