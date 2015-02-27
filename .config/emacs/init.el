;; (setq url-proxy-services '(("http" . "172.16.1.130:8080")))
(add-to-list 'load-path "~/.config/emacs/")
(require 'sys/packages)

(setq cfg-var:work-dir "~/.local/share/emacs")
(setq cfg-var:theme "light") ;; TODO: test after load session
(setq cfg-var:find-file 'ido-find-file)
(setq cfg-var:find-buffer 'ido-switch-buffer)
(setq cfg-var:find-command 'smex)
(setq cfg-var:find-major-command 'smex-major-mode-commands)

(require 'ui-config)
(require 'edit-config)
(require 'menu-config)
;;(require 'cedet-config)
(require 'python-lang)
(require 'rust-lang)
(require 'org-config)
(require 'hotkeys)

(cfg:init)
