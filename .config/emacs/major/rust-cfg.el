(provide 'major/rust-cfg)

(defun lcl:rust-cargo (arg)
  (require 'compile)
  (compile (format "cargo %s" arg)))

(defun lcl:rust-defun-compile (name arg)
  `(defun ,name ()
     (interactive)
     (lcl:rust-cargo ,arg)))

(defvar cfg:rust-compile-list '(("rust-run-release" . "run --release")
                                ("rust-run-debug" . "run")
                                ("rust-run-test" . "test")))

(defmacro lcl:rust-compile-func-generator ()
  `(progn ,@(mapcar
             (lambda (x) (lcl:rust-defun-compile (intern (car x)) (cdr x)))
             cfg:rust-compile-list)))

(lcl:rust-compile-func-generator)

(defadvice compile (around lcl:rust-before-compile (command &optional comint))
  (message "!!!my defadvice")
  (setq rust-compile-command command)
  ad-do-it
  (setq rust-compile-command nil)
  )

(defun lcl:rust-compile-command ()
  (if rust-compile-command
      rust-compile-command
    (format "cargo %s" (cdr
                        (assoc
                         (ido-completing-read "action: " (mapcar #'car cfg:rust-compile-list))
                         cfg:rust-compile-list))))
  )

(defun lcl:rust-compile ()
  (set (make-local-variable 'rust-compile-command) nil)
  (set (make-local-variable 'compile-command) '(lcl:rust-compile-command)))

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
