(provide 'minor/yasnippet-cfg)

(defun cfg:yasnippet ()
  (require 'yasnippet)
  (yas-reload-all))

(add-hook 'cfg-hook:minor-mode 'cfg:yasnippet)

(cfg:add-package 'yasnippet)
