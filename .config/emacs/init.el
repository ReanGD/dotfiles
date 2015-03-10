;; (setq url-proxy-services '(("http" . "172.16.1.130:8080")))
(add-to-list 'load-path "~/.config/emacs/")
(require 'sys/packages)

(setq cfg-var:work-dir "~/.local/share/emacs")
(setq cfg-var:theme "light") ;; TODO: test after load session
(setq cfg-var:find-file 'ido-find-file)
(setq cfg-var:find-buffer 'ido-switch-buffer)
(setq cfg-var:find-command 'smex)
(setq cfg-var:find-major-command 'smex-major-mode-commands)
(setq cfg-var:autocomplete 'company-complete-common)
(setq cfg-var:use-irony t)

(require 'major/cmake-cfg)
(require 'major/cpp-cfg)

(require 'minor/ido-cfg)
;;(require 'minor/helm-cfg)
;;(require 'minor/auto-complete-cfg)
(require 'minor/company-cfg)
(require 'minor/projectile-cfg)
(if cfg-var:use-irony
    (require 'minor/irony-cfg))
;;(require 'minor/cpputils-cmake-cfg)
(require 'minor/test-cfg)

(require 'ui-config)
(require 'edit-config)
;;(require 'cedet-config)
(require 'python-lang)
(require 'rust-lang)
(require 'org-config)
(require 'hotkeys)

(cfg:init)
