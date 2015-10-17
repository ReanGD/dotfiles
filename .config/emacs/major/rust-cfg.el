(provide 'major/rust-cfg)

;; (defun lcl:rust-cargo (arg)
;;   (require 'compile)
;;   (compile (format "cargo %s" arg)))

;; (defun lcl:rust-defun-compile (name arg)
;;   `(defun ,name ()
;;      (interactive)
;;      (lcl:rust-cargo ,arg)))

;; (defmacro lcl:rust-compile-func-generator ()
;;   `(progn ,@(mapcar
;;              (lambda (x) (lcl:rust-defun-compile (intern (car x)) (cdr x)))
;;              cfg:rust-compile-list)))

;; (lcl:rust-compile-func-generator)

(defun cfg:rust ()
  (autoload 'rust-mode "rust-mode" nil t)
  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))

  (add-hook 'rust-mode-hook
            (lambda ()
              (setq indent-tabs-mode nil
                    tab-width 4
                    rust-indent-offset 4
                    rustfmt-bin "/home/rean/tmp/rustfmt/target/release/rustfmt")
              ))

  (setq racer-rust-src-path "/usr/src/rust/src")
  (setq racer-cmd "/usr/bin/racer")

  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'rust-mode-hook #'lcl:rust-compile)
  (add-hook 'racer-mode-hook #'eldoc-mode)

  (add-hook 'racer-mode-hook #'company-mode)
  (setq company-tooltip-align-annotations t)
  )
(add-hook 'cfg-hook:major-mode 'cfg:rust)

(cfg:add-package 'rust-mode)
(cfg:add-package 'racer)
;; (cfg:add-package 'rustfmt)
