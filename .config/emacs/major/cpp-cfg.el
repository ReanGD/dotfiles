(provide 'major/cpp-cfg)


(defun lcl:inside-class-enum-p (pos)
  "Checks if POS is within the braces of a C++ \"enum class\"."
  (ignore-errors
    (save-excursion
      (goto-char pos)
      (up-list -1)
      (backward-sexp 1)
      (looking-back "enum[ \t]+class[ \t]+[^}]*"))))

(defun lcl:align-enum-class (langelem)
  (if (lcl:inside-class-enum-p (c-langelem-pos langelem))
      0
    (c-lineup-topmost-intro-cont langelem)))

(defun lcl:align-enum-class-closing-brace (langelem)
  (if (lcl:inside-class-enum-p (c-langelem-pos langelem))
      '-
    '+))

(defun lcl:fix-enum-class ()
  "Setup `c++-mode' to better handle \"class enum\"."
  (add-to-list 'c-offsets-alist '(topmost-intro-cont . lcl:align-enum-class))
  (add-to-list 'c-offsets-alist
               '(statement-cont . lcl:align-enum-class-closing-brace))) 

(defun lcl:cpp-hook ()
  (setq c-default-style "Ellemtel" ;; code-slyle bsd
        indent-tabs-mode nil       ;; отступы делаются пробелами	
        tab-width 4                ;; default tab width
        c-basic-offset 4           ;; tab-width for cpp-lang (js, php, c++, java) can be eq tab-width	
        ))

(defun lcl:cpp-compile-hook ()
  (require 'compile)
  (setq make-dir (locate-dominating-file (buffer-file-name) "Makefile"))
  (set (make-local-variable 'compile-command)
       (format "make --no-print-directory -C %s" make-dir)))

(defun cfg:cpp ()
  (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
  (add-hook 'c-mode-common-hook 'lcl:cpp-hook)
  (add-hook 'c-mode-common-hook 'lcl:cpp-compile-hook)
  (add-hook 'c++-mode-hook 'lcl:fix-enum-class)
  )
(add-hook 'cfg-hook:major-mode 'cfg:cpp)
