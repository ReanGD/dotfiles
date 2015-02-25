(setq
  user-emacs-directory "~/.local/share/emacs"
  backup-directory-alist `(("." . "~/.local/share/emacs/backup"))
  auto-save-list-file-prefix "~/.local/share/emacs/auto-save-list/.saves")

(add-to-list 'load-path "~/.config/emacs/")

;; (setq url-proxy-services '(("http" . "172.16.1.130:8080")))
(require 'sys/packages)

(setq cfg-var:theme "light") ;; TODO: test after load session
(setq cfg-var:find-file 'ido-find-file)
(setq cfg-var:find-buffer 'ido-switch-buffer)
(setq cfg-var:find-command 'smex)
(setq cfg-var:find-major-command 'smex-major-mode-commands)

(cfg:init '("ui-config" ;;!!
	    "edit-config"
	    "menu-config";;!!
	    ;;"cedet-config"
	    "python-lang"
	    "rust-lang"
	    "org-config"
	    "hotkeys"
	    "rus-lang"))
