;;; go-cfg.el --- Configure golang
;;; Commentary:
;;; Code:
(require 'sys/packages)
(require 'company nil t)
(require 'flycheck nil t)
(require 'yasnippet nil t)

(defun cfg:go ()
  "Configure golang."
  (add-hook 'before-save-hook 'gofmt-before-save)
  (setq-default gofmt-command "goimports")
  (add-hook 'go-mode-hook 'yas-minor-mode)
  (add-hook 'go-mode-hook 'flycheck-mode)
  (add-hook 'go-mode-hook 'go-eldoc-setup)
  (add-hook 'go-mode-hook (lambda ()
                            (set (make-local-variable 'company-backends) '(company-go))
                            (company-mode)))
  (load-file "$GOPATH_BASE/src/golang.org/x/tools/cmd/oracle/oracle.el")
  )
(add-hook 'cfg-hook:major-mode 'cfg:go)

(cfg:add-package 'go-mode)
(cfg:add-package 'go-eldoc)
(cfg:add-package 'go-rename)
(cfg:add-package 'company-go)

(provide 'major/go-cfg)
;;; go-cfg.el ends here
