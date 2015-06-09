(provide 'python-lang)


(defun cfg:python ()
  (elpy-enable))
(add-hook 'cfg-hook:major-mode 'cfg:python)

(defun cfg:python3 ()
  (setq python-check-command "flake8"
        python-shell-interpreter "python"
        elpy-rpc-python-command "python"))

(defun cfg:python2 ()
  (setq python-check-command "flake8-python2"
        python-shell-interpreter "python2"
        elpy-rpc-python-command "python2"))
(cfg:add-package 'elpy)
