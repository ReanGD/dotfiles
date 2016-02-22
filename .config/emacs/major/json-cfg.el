;;; json-cfg.el --- Configure json
;;; Commentary:
;;; Code:
(require 'sys/packages)
(require 'lua-mode nil t)

(defun cfg:json ()
  "Configure json."
  (require 'json-mode)
  (add-to-list 'auto-mode-alist '("\\.json$" . json-mode))
  )
(add-hook 'cfg-hook:major-mode 'cfg:json)

(cfg:add-package 'json-mode)

(provide 'major/json-cfg)
;;; json-cfg.el ends here
