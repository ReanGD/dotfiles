(setq
  user-emacs-directory "~/.local/share/emacs"
  backup-directory-alist `(("." . "~/.local/share/emacs/backup"))
  auto-save-list-file-prefix "~/.local/share/emacs/auto-save-list/.saves")

(add-to-list 'load-path "~/.config/emacs/")
(add-to-list 'load-path "~/.config/emacs/lib/")

;; (setq url-proxy-services '(("http" . "172.16.1.130:8080")))
(require 'lib-package)

(setq user-module '("ui-config"
		    "edit-config"
		    "menu-config"
		    "python-lang"
		    "rust-lang"
		    "org-config"
		    "hotkeys"
		    "rus-lang"))

(lib-package--initialize)
(lib-package--string-list-require user-module)
(lib-package--on-load-list user-module)
(lib-package--install-packages user-module)
(lib-package--on-configure-list user-module)
(lib-package--on-exit-list user-module)
