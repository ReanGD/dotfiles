(provide 'major/go-cfg)


;; flycheck
;; yasnippet
(defun cfg:go ()
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save)
  (add-hook 'go-mode-hook 'go-eldoc-setup)
  (add-hook 'go-mode-hook (lambda ()
                            (set (make-local-variable 'company-backends) '(company-go))
                            (company-mode)))
  (load-file "$GOPATH/src/golang.org/x/tools/cmd/oracle/oracle.el")
  )
(add-hook 'cfg-hook:major-mode 'cfg:go)

(cfg:add-package 'go-mode)
(cfg:add-package 'go-eldoc)
(cfg:add-package 'go-rename)
(cfg:add-package 'company-go)
