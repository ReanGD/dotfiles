;;; docker-cfg.el --- Configure docker
;;; Commentary:
;;; Code:
(require 'sys/packages)
(require 'dockerfile-mode nil t)

(defun cfg:docker ()
  "Configure docker."
  (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))
  )
(add-hook 'cfg-hook:major-mode 'cfg:docker)

(cfg:add-package 'dockerfile-mode)

(provide 'major/docker-cfg)
;;; docker-cfg.el ends here
