(provide 'minor/company-cfg)


(defun cfg:company ()
  (require 'company)
  (global-company-mode t)
  (setq company-minimum-prefix-length 3)
  (company-quickhelp-mode 1)
  (require 'company-statistics)
  (setq company-statistics-size 1000)
  (company-statistics-mode))
(add-hook 'cfg-hook:minor-mode 'cfg:company)

(cfg:add-package 'company)
(cfg:add-package 'company-quickhelp)
(cfg:add-package 'company-statistics)
(if cfg-var:use-irony
    (cfg:add-package 'company-irony))
