(provide 'minor/flycheck-cfg)

(defun cfg:flycheck-emacs-lisp ()
  (add-hook 'emacs-lisp-mode-hook 'flycheck-mode))

(defun cfg:flycheck ()
  (require 'flycheck)
  (setq-default flycheck-emacs-lisp-load-path load-path)
  (cfg:flycheck-emacs-lisp)
  )
(add-hook 'cfg-hook:minor-mode 'cfg:flycheck)

(cfg:add-package 'flycheck)
(cfg:add-package 'flycheck-package)
