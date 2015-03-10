(provide 'minor/test-cfg)


(defun cfg:test ())
(add-hook 'cfg-hook:minor-mode 'cfg:test)

;; (cfg:add-package 'smart-tabs-mode)
