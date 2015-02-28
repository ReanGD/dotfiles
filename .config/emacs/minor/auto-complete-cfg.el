(provide 'minor/auto-complete-cfg)


(defun cfg:auto-complete ()
  (require 'auto-complete-config)
  (ac-config-default)
  (global-auto-complete-mode t))
(add-hook 'cfg-hook:minor-mode 'cfg:auto-complete)

(cfg:add-package 'auto-complete)
