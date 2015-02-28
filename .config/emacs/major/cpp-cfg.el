(provide 'major/cpp-cfg)

(defun lcl:cpp-hook ()
  (local-unset-key (kbd "M-j"))
  (local-unset-key (kbd "C-M-j"))
)

(defun cfg:cpp ()
  (add-hook 'c-mode-common-hook 'lcl:cpp-hook))
(add-hook 'cfg-hook:major-mode 'cfg:cpp)
