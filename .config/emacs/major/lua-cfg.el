(provide 'major/lua-cfg)


(defun cfg:lua ()
  (autoload 'lua-mode "lua-mode" "Lua editing mode." t)
  (add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
  (add-to-list 'interpreter-mode-alist '("lua" . lua-mode)))
(add-hook 'cfg-hook:major-mode 'cfg:lua)

(cfg:add-package 'lua-mode)
