(provide 'minor/yasnippet-cfg)

(defun cfg:yasnippet ()
  (require 'yasnippet)
  (yas-global-mode 1))

(add-hook 'cfg-hook:minor-mode 'cfg:yasnippet)

(cfg:add-package 'yasnippet)
