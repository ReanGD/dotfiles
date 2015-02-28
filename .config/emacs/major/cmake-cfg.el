(provide 'major/cmake-cfg)


(defun cfg:cmake ()
  (require 'cmake-mode)
  (cmake-mode))
(add-hook 'cfg-hook:major-mode 'cfg:cmake)

(cfg:add-package 'cmake-mode)
