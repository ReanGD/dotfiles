(setq
  user-emacs-directory "~/.local/share/emacs"
  backup-directory-alist `(("." . "~/.local/share/emacs/backup"))
  auto-save-list-file-prefix "~/.local/share/emacs/auto-save-list/.saves")

(add-to-list 'load-path "~/.config/emacs/")

;; (setq url-proxy-services '(("http" . "172.16.1.130:8080")))
(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/"))
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t))

(require 'cl)

(setq base-package '())

(require 'ui-config)
(setq base-package (append base-package (ui-config-packages)))

(require 'edit-config)
(setq base-package (append base-package (edit-config-packages)))

(require 'menu-config)
(setq base-package (append base-package (menu-config-packages)))

(require 'python-lang)
(setq base-package (append base-package (python-lang-packages)))

(require 'rust-lang)
(setq base-package (append base-package (rust-lang-packages)))

(require 'org-config)
(setq base-package (append base-package (org-config-packages)))

(require 'hotkeys)
(setq base-package (append base-package (hotkeys-packages)))

(require 'rus-lang)
(setq base-package (append base-package (rus-lang-packages)))


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

(ui-config-init)
(edit-config-init)
(menu-config-init)
(python-lang-init)
(rust-lang-init)
(org-config-init)
(hotkeys-init)
(rus-lang-init)
