(provide 'major/markdown-cfg)


(defun cfg:markdown ())
(add-hook 'cfg-hook:minor-mode 'cfg:markdown)

(cfg:add-package 'markdown-mode)
