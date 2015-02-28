(provide 'minor/company-cfg)


(defun cfg:company ()
  (require 'company)
  (global-company-mode t))
  (setq company-minimum-prefix-length 3)
  ;;(company-mode)
(add-hook 'cfg-hook:minor-mode 'cfg:company)

(cfg:add-package 'company)
