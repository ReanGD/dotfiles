(provide 'init-pkg)

(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/"))
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
  )

(require 'cl)

(setq base-package
  '(solarized-theme move-dup yascroll powerline powerline-evil auto-complete python-environment autopair
    flycheck flycheck-color-mode-line nose helm python-mode))

(require 'rust-lang)
(setq base-package (append base-package (rust-lang-packages)))

(defvar my-packages base-package)
 
(defun my-packages-installed-p ()
  (loop for p in my-packages
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))
 
(unless (my-packages-installed-p)
  (package-refresh-contents)
  (dolist (p my-packages)
    (when (not (package-installed-p p))
      (package-install p))))

(if (not (package-installed-p 'jedi))
   (progn
     (package-refresh-contents)
     (package-install 'jedi)
     (setq jedi:environment-virtualenv
       (list "virtualenv2" "--system-site-packages"))
     (jedi:install-server)))
