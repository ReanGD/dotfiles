;;; yaml-cfg.el --- Configure yaml
;;; Commentary:
;;; Code:
(require 'sys/packages)
(require 'yaml-mode nil t)

(defun cfg:yaml ()
  "Configure yaml."
  (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
  )
(add-hook 'cfg-hook:major-mode 'cfg:yaml)

(cfg:add-package 'yaml-mode)

(provide 'major/yaml-cfg)
;;; yaml-cfg.el ends here
