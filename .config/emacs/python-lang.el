(provide 'python-lang)


;; Ignoring electric indentation
(defun lcl:electric-indent-ignore-python (char)
  "Ignore electric indentation for python-mode"
  (if (equal major-mode 'python-mode)
      `no-indent'
    nil))

(defun lcl:python-hotkeys ()
  (local-set-key (kbd "RET") 'newline-and-indent) ;; py-newline-and-indent
  )

(defun cfg:python ()
  (add-hook 'electric-indent-functions 'lcl:electric-indent-ignore-python)
  (add-hook 'python-mode-hook 'lcl:python-hotkeys)
  (autoload 'python-mode "python-mode" "Python Mode." t)
  (add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
  (add-to-list 'interpreter-mode-alist '("python" . python-mode)))
(add-hook 'cfg-hook:major-mode 'cfg:python)

(defun cfg:jedi ()
  (setq jedi:environment-virtualenv
	(list "virtualenv2" "--system-site-packages"))
  (add-hook 'python-mode-hook 'jedi:setup)
  (setq jedi:complete-on-dot t))
(add-hook 'cfg-hook:minor-mode 'cfg:jedi)

(defun cfg:flycheck ()
  (add-hook 'after-init-hook #'global-flycheck-mode)
  (setq flycheck-flake8rc "/home/rean/.config/python/flake8.cfg")
  (setq-default flycheck-disabled-checkers '(emacs-lisp emacs-lisp-checkdoc))
  (require 'flycheck-color-mode-line)
  (eval-after-load "flycheck"
    '(add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode)))
(add-hook 'cfg-hook:minor-mode 'cfg:flycheck)

(defun cfg:nose ()
  (require 'nose)
  (setq nose-global-name "nosetests2")
  (add-hook 'python-mode-hook (lambda () (nose-mode t)))
  (define-key nose-mode-map (kbd "M-c n m") 'nosetests-module)
  (define-key nose-mode-map (kbd "M-c n .") 'nosetests-one))
(add-hook 'cfg-hook:minor-mode 'cfg:nose)

(defun cfg:install-jedi ()
  (setq jedi:environment-virtualenv
	(list "virtualenv2" "--system-site-packages"))
  (jedi:install-server))

(cfg:add-package 'python-environment)
(cfg:add-package 'flycheck)
(cfg:add-package 'flycheck-color-mode-line)
(cfg:add-package 'nose)
(cfg:add-package 'python-mode)
(cfg:add-package 'jedi 'cfg:install-jedi)
