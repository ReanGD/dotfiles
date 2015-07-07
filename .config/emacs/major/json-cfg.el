(provide 'major/json-cfg)

(defun cfg:json ()
  (require 'json-mode)
  (add-to-list 'auto-mode-alist '("\\.json$" . json-mode))
  )
(add-hook 'cfg-hook:major-mode 'cfg:json)

(cfg:add-package 'json-mode)
