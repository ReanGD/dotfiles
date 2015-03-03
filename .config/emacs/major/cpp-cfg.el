(provide 'major/cpp-cfg)

(defun* get-closest-pathname (&optional (file "Makefile"))
  "Determine the pathname of the first instance of FILE starting from the current directory towards root.
This may not do the correct thing in presence of links. If it does not find FILE, then it shall return the name
of FILE in the current directory, suitable for creation"
  (let ((root (expand-file-name "/"))) ; the win32 builds should translate this correctly
    (expand-file-name file
		      (loop 
			for d = default-directory then (expand-file-name ".." d)
			if (file-exists-p (expand-file-name file d))
			return d
			if (equal d root)
			return nil))))

(defun lcl:cpp-hook ()
  (setq c-default-style "bsd"   ;; code-slyle
	indent-tabs-mode nil    ;; отступы делаются пробелами	
	tab-width 4             ;; default tab width
	c-basic-offset 4        ;; tab-width for cpp-lang (js, php, c++, java) can be eq tab-width	
	)
  (local-unset-key (kbd "M-j"))
  (local-unset-key (kbd "C-M-j")))

(defun lcl:cpp-compile-hook ()
  (require 'compile)
  (setq make-dir (file-name-directory (directory-file-name (get-closest-pathname))))
  (set (make-local-variable 'compile-command)
       (format "make --no-print-directory -C %s" make-dir)))

(defun cfg:cpp ()
  (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
  (add-hook 'c-mode-common-hook 'lcl:cpp-hook)
  (add-hook 'c-mode-common-hook 'lcl:cpp-compile-hook)
  )
(add-hook 'cfg-hook:major-mode 'cfg:cpp)
