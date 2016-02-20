;;; lua-cfg.el --- Configure lua
;;; Commentary:
;;; Code:
(require 'sys/packages)
(require 'lua-mode nil t)

(defun cfg:lua ()
  "Configure lua."
  (add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
  (add-to-list 'interpreter-mode-alist '("lua" . lua-mode)))
(add-hook 'cfg-hook:major-mode 'cfg:lua)

(cfg:add-package 'lua-mode)

(provide 'major/lua-cfg)
;;; lua-cfg.el ends here
