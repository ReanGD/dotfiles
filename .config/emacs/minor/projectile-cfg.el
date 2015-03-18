(provide 'minor/projectile-cfg)


(defun lcl:projectile-hotkeys ()
  (define-key projectile-mode-map (kbd "M-m s") 'projectile-find-other-file)
  (define-key projectile-mode-map (kbd "M-m o") 'projectile-find-file)
  )

(defun cfg:projectile ()
  (projectile-global-mode)
  (setq projectile-enable-caching t)
  (lcl:projectile-hotkeys))
(add-hook 'cfg-hook:minor-mode 'cfg:projectile)

(cfg:add-package 'projectile)
