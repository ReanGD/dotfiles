(provide 'major/go-cfg)


(defun cfg:go ()
  (add-hook 'before-save-hook 'gofmt-before-save)
  (add-hook 'go-mode-hook 'go-eldoc-setup)
  (add-hook 'go-mode-hook (lambda ()
                            (set (make-local-variable 'company-backends) '(company-go))
                            (company-mode)))
  )
(add-hook 'cfg-hook:major-mode 'cfg:go)

(cfg:add-package 'go-mode)
(cfg:add-package 'go-eldoc)
(cfg:add-package 'company-go)
