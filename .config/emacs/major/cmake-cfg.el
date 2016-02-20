;;; cmake-cfg.el --- Configure cmake
;;; Commentary:
;;; Code:
(require 'sys/packages)
(require 'cmake-mode nil t)

(defun cfg:cmake ()
  "Configure cmake."
  (cmake-mode))
(add-hook 'cfg-hook:major-mode 'cfg:cmake)

(cfg:add-package 'cmake-mode)

(provide 'major/cmake-cfg)
;;; cmake-cfg.el ends here
