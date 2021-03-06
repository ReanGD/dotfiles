(provide 'minor/company-cfg)


(defun cfg:company ()
  (setq-default company-lighter " cmp")
  (require 'company)
  (setq company-tooltip-limit 10
        company-idle-delay .5
        company-echo-delay 0
        company-minimum-prefix-length 3)
  (company-quickhelp-mode 1)
  (if cfg-var:use-irony
      (add-to-list 'company-backends 'company-irony))
  (setq company-tooltip-align-annotations t)

  (require 'company-c-headers)
  (add-to-list 'company-c-headers-path-system "/usr/lib/clang/3.5.1/include")
  ;; (add-to-list 'company-c-headers-path-user "/usr/lib/clang/3.5.1/include")
  (add-to-list 'company-backends 'company-c-headers)

  (require 'company-statistics)
  (setq company-statistics-size 1000)
  (company-statistics-mode)
  )
(add-hook 'cfg-hook:minor-mode 'cfg:company)

(cfg:add-package 'company)
(cfg:add-package 'company-quickhelp)
(cfg:add-package 'company-statistics)
(cfg:add-package 'company-c-headers)
(if cfg-var:use-irony
    (cfg:add-package 'company-irony))
