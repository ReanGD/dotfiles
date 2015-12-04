(provide 'major/go-cfg)


(defun cfg:go ()
  )
(add-hook 'cfg-hook:major-mode 'cfg:go)

(cfg:add-package 'go-mode)
