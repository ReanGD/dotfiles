(provide 'major/cmake-cfg)


(defun cfg:cmake ()
  (require 'cmake-mode)
  (cmake-mode)
  ;(setq auto-mode-alist
;	  (append
;	   '(("CMakeLists\\.txt\\'" . cmake-mode))
;	   '(("\\.cmake\\'" . cmake-mode))
;	   auto-mode-alist))
  )

(add-hook 'cfg-hook:major-mode 'cfg:cmake)

(cfg:add-package 'cmake-mode)
