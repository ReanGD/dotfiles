(provide 'minor/multi-compile-cfg)

(defun cfg:multi-compile ()
  )
(add-hook 'cfg-hook:minor-mode 'cfg:multi-compile)

(cfg:add-package 'multi-compile)
