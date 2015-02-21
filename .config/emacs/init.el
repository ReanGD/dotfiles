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
(require 'edit-config)
(require 'menu-config)
(require 'python-lang)
(require 'rust-lang)
(require 'org-config)
(require 'hotkeys)
(require 'rus-lang)

(ui-config-on-load)
(edit-config-on-load)
(menu-config-on-load)
(python-lang-on-load)
(rust-lang-on-load)
(org-config-on-load)
(hotkeys-on-load)
(rus-lang-on-load)

(setq base-package (append base-package (ui-config-packages)))
(setq base-package (append base-package (edit-config-packages)))
(setq base-package (append base-package (menu-config-packages)))
(setq base-package (append base-package (python-lang-packages)))
(setq base-package (append base-package (rust-lang-packages)))
(setq base-package (append base-package (org-config-packages)))
(setq base-package (append base-package (hotkeys-packages)))
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
