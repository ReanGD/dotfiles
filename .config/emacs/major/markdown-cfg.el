;;; go-cfg.el --- Configure markdown
;;; Commentary:
;;; Code:
(require 'sys/packages)
(require 'markdown-mode nil t)

(defun cfg:markdown ()
  "Configure markdown."
  (add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode)))
(add-hook 'cfg-hook:minor-mode 'cfg:markdown)

(cfg:add-package 'markdown-mode)
(cfg:add-package 'gh-md)

(provide 'major/markdown-cfg)
;;; markdown-cfg.el ends here
