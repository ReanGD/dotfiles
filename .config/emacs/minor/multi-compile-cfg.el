(provide 'minor/multi-compile-cfg)

(defun cfg:multi-compile ()
  (require 'multi-compile)
  (add-to-list 'multi-compile-alist
               '(go-mode . (
                            ("go-run" . "go run %path")
                            )))
  )
(add-hook 'cfg-hook:minor-mode 'cfg:multi-compile)

(cfg:add-package 'multi-compile)
