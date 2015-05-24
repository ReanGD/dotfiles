(provide 'minor/irony-cfg)

(defun my-load-irony-options ()
  (irony-cdb-json-add-compile-commands-path (projectile-project-root)
                        (concat (projectile-project-root) "compile_commands.json"))
  (irony-cdb-autosetup-compile-options))
  ;; (setq make-dir (locate-dominating-file (buffer-file-name) "Makefile"))
  ;; (set (make-local-variable 'compile-command)
  ;;      (format "make --no-print-directory -C %s" make-dir)))

(defun my-irony-mode-hook ()
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async))

(defun cfg:irony ()
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  
  (add-hook 'c++-mode-hook 'my-load-irony-options)
  (add-hook 'c-mode-hook 'my-load-irony-options)
  
  (add-hook 'irony-mode-hook 'my-irony-mode-hook)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
  (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)
  )
(add-hook 'cfg-hook:minor-mode 'cfg:irony)

(cfg:add-package 'irony)
