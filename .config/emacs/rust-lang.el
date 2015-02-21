(provide 'rust-lang)

(defun rust-lang-packages ()
  '(rust-mode flycheck-rust company))
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

(defun auto-complite ()
  (setq racer-rust-src-path "/home/rean/tmp/rust/src")
  (setq racer-cmd "/usr/bin/racer")
  ;; (add-to-list 'load-path "<path-to-racer>/editors")
  (eval-after-load "rust-mode" '(require 'racer))
  )


(defun rust-lang-on-load ()
  )


(defun rust-lang-init ()
  (autoload 'rust-mode "rust-mode" nil t)
  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
  (auto-complite)
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
  (add-hook 'rust-mode-hook
	    (lambda ()
	      (define-key rust-mode-map (kbd "<f5>") 'rust-save-compile-and-run)))
  )
