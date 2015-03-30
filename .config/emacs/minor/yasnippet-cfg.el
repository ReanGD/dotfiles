(provide 'minor/yasnippet-cfg)

(defun lcl:yasnippet-hotheys()
  (local-unset-key (kbd "C-i"))
  )

(defun cfg:yasnippet ()
  (require 'yasnippet)
  (add-hook 'yas-minor-mode-hook 'lcl:yasnippet-hotheys)
  (yas-global-mode 1))

(add-hook 'cfg-hook:minor-mode 'cfg:yasnippet)

(cfg:add-package 'yasnippet)
