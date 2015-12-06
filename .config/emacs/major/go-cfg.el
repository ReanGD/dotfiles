(provide 'major/go-cfg)


(defun cfg:go ()
  (add-hook 'before-save-hook 'gofmt-before-save)
  )
(add-hook 'cfg-hook:major-mode 'cfg:go)

(cfg:add-package 'go-mode)
