(provide 'minor/projectile-cfg)


(defun cfg:projectile ()
  (projectile-global-mode)
  (setq projectile-enable-caching t))
(add-hook 'cfg-hook:minor-mode 'cfg:projectile)

(cfg:add-package 'projectile)
