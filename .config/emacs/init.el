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

(cfg:pre-init)

(require 'major/go-cfg)
;; warnings:
(require 'major/cmake-cfg)
(require 'major/cpp-cfg)
(require 'major/markdown-cfg)
(require 'major/lua-cfg)
(require 'major/org-cfg)
(require 'major/yaml-cfg)
(require 'major/json-cfg)
(require 'major/python-cfg)
(require 'major/rust-cfg)

(require 'minor/cfg-cfg)
(require 'minor/mode-line-cfg)
(require 'minor/wrapline-cfg)
(require 'minor/window-num-cfg)
(require 'minor/ido-cfg)
(require 'minor/flycheck-cfg)
(require 'minor/company-cfg)
(require 'minor/yasnippet-cfg)
(require 'minor/projectile-cfg)
(require 'minor/test-cfg)
(require 'minor/search-cfg)
(require 'minor/spelling-cfg)
(require 'minor/multi-compile-cfg)

(if cfg-var:use-irony
    (require 'minor/irony-cfg))
;;(require 'minor/helm-cfg)
;;(require 'minor/auto-complete-cfg)
;;(require 'minor/cpputils-cmake-cfg)

(require 'ui-config)
(require 'edit-config)
(require 'hotkeys)
;;(require 'cedet-config)

(cfg:post-init)
