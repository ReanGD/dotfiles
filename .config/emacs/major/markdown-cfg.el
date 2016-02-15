(provide 'major/markdown-cfg)


(defun cfg:markdown ()
  (require 'markdown-mode)
  (add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
  )
(add-hook 'cfg-hook:minor-mode 'cfg:markdown)

(cfg:add-package 'markdown-mode)
