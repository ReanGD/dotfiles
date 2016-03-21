;;; cpp-cfg.el --- Configure cpp
;;; Commentary:
;;; Code:
(require 'cc-vars)

(defun cfg:cpp ()
  "Configure cpp."
  (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

  (add-hook 'c-mode-common-hook
            (lambda ()
              (setq c-default-style "Ellemtel" ;; code-slyle bsd
                    indent-tabs-mode nil       ;; отступы делаются пробелами
                    tab-width 4                ;; default tab width
                    c-basic-offset 4           ;; tab-width for cpp-lang (js, php, c++, java) can be eq tab-width
                    )))
  )
(add-hook 'cfg-hook:major-mode 'cfg:cpp)

(provide 'major/cpp-cfg)
;;; cpp-cfg.el ends here
