(setq
  user-emacs-directory "~/.local/share/emacs"
  backup-directory-alist `(("." . "~/.local/share/emacs/backup"))
  auto-save-list-file-prefix "~/.local/share/emacs/auto-save-list/.saves")

(add-to-list 'load-path "~/.config/emacs/")
(add-to-list 'load-path "~/.config/emacs/lib/")

;; (setq url-proxy-services '(("http" . "172.16.1.130:8080")))
(require 'lib-package)
(lib-package-init '("ui-config"
		    "edit-config"
		    "menu-config"
		    "python-lang"
		    "rust-lang"
		    "org-config"
		    "hotkeys"
		    "rus-lang"))
