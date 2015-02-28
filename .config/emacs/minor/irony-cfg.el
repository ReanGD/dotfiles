(provide 'minor/irony-cfg)


(defun cfg:irony ()
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode))
(add-hook 'cfg-hook:minor-mode 'cfg:irony)

(cfg:add-package 'irony)
