;;; popwin-cfg.el --- Configure popwin
;;; Commentary:
;;; Code:
(require 'sys/packages)
(require 'popwin nil t)

(defun cfg:popwin ()
  "Configure popwin."
  (popwin-mode 1))
(add-hook 'cfg-hook:minor-mode 'cfg:popwin)

(cfg:add-package 'popwin)

(provide 'minor/popwin-cfg)
;;; popwin-cfg.el ends here
