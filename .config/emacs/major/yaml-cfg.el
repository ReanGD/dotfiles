(provide 'major/yaml-cfg)

(defun cfg:yaml ()
  (require 'yaml-mode)
  (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
  )
(add-hook 'cfg-hook:major-mode 'cfg:yaml)

(cfg:add-package 'yaml-mode)
