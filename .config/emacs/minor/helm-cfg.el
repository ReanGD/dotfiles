(provide 'minor/helm-cfg)


;; package "helm" - аналог ido
(defun cfg:helm ()
  (require 'helm-config)
  (helm-mode t)
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  (setq helm-M-x-fuzzy-match t))
(add-hook 'cfg-hook:minor-mode 'cfg:helm)

(cfg:add-package 'helm)
