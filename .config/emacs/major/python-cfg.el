(provide 'major/python-cfg)


(defun cfg:python ()
  (setq elpy-modules
        '(elpy-module-sane-defaults
          elpy-module-company
          elpy-module-eldoc
          elpy-module-flymake
          ;; elpy-module-highlight-indentation
          elpy-module-pyvenv
          elpy-module-yasnippet))
  (elpy-enable)
  )
(add-hook 'cfg-hook:major-mode 'cfg:python)

(defun cfg:python3 ()
  (interactive)
  (setq python-check-command "flake8"
        python-shell-interpreter "python"
        elpy-rpc-python-command "python"))

(defun cfg:python2 ()
  (interactive)
  (setq python-check-command "flake8-python2"
        python-shell-interpreter "python2"
        elpy-rpc-python-command "python2"))
(cfg:add-package 'elpy)
