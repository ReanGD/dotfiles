(provide 'minor/wrapline-cfg)

(defun cfg:wrapline ()
  ;; (adaptive-wrap-prefix-mode)
  ;; (visual-line-mode)
  (global-visual-line-mode t)
  ;; (setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
  ;; (addvisual-line-mode-hook
  ;; (setq word-wrap t)
)
(add-hook 'cfg-hook:minor-mode 'cfg:wrapline)

;; (cfg:add-package 'adaptive-wrap)
(cfg:add-package 'olivetti)
