(provide 'rust-lang)

(defun rust-lang-packages ()
  '(rust-mode flycheck-rust flymake-rust))
;; melpa: flymake-rust

;; http://emacs.stackexchange.com/questions/2137/press-f5-to-save-compile-and-run-current-rust-file
(defun rust-save-compile-and-run ()
  (interactive)
  (save-buffer)
  (if (locate-dominating-file (buffer-file-name) "Cargo.toml")
    (compile "cargo run")
    (compile
      (format "rustc %s & %s"
        (buffer-file-name)
        (file-name-sans-extension (buffer-file-name))))))

(defun rust-lang-init ()
  (autoload 'rust-mode "rust-mode" nil t)
  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
  (add-hook 'rust-mode-hook
	    (lambda ()
	      (define-key rust-mode-map (kbd "<f5>") 'rust-save-compile-and-run)))
  )


