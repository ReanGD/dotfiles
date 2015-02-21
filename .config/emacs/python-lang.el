(provide 'python-lang)


;; Ignoring electric indentation
(defun electric-indent-ignore-python (char)
  "Ignore electric indentation for python-mode"
  (if (equal major-mode 'python-mode)
      `no-indent'
    nil))

;; Enter key executes newline-and-indent
(defun set-newline-and-indent ()
  "Map the return key with `newline-and-indent'"
  (local-set-key (kbd "RET") 'newline-and-indent))

(defun python-settings ()
  (add-hook 'electric-indent-functions 'electric-indent-ignore-python)
  (add-hook 'python-mode-hook 'set-newline-and-indent)
  (autoload 'python-mode "python-mode" "Python Mode." t)
  (add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
  (add-to-list 'interpreter-mode-alist '("python" . python-mode)))

(defun jedi-settings ()
  (setq jedi:environment-virtualenv
	(list "virtualenv2" "--system-site-packages"))
  (add-hook 'python-mode-hook 'jedi:setup)
  (setq jedi:complete-on-dot t))

(defun flycheck-settings ()
  (add-hook 'after-init-hook #'global-flycheck-mode)
  (setq flycheck-flake8rc "/home/rean/.config/python/flake8.cfg")
  (setq-default flycheck-disabled-checkers '(emacs-lisp emacs-lisp-checkdoc))
  (require 'flycheck-color-mode-line)
  (eval-after-load "flycheck"
    '(add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode)))

(defun nose-settings ()
  (require 'nose)
  (setq nose-global-name "nosetests2")
  (add-hook 'python-mode-hook (lambda () (nose-mode t)))
  (define-key nose-mode-map (kbd "M-c n m") 'nosetests-module)
  (define-key nose-mode-map (kbd "M-c n .") 'nosetests-one))

(defun install-jedi ()
  (setq jedi:environment-virtualenv
	(list "virtualenv2" "--system-site-packages"))
  (jedi:install-server))

;; -------------------- hooks --------------------

(defun python-lang-packages ()
  '(python-environment flycheck flycheck-color-mode-line nose python-mode (jedi install-jedi)))

;(list jedi 'install-jedi)

(defun python-lang-on-configure ()
  (python-settings)
  (jedi-settings)
  (flycheck-settings)
  (nose-settings))

(defun python-lang-on-exit ()
  )
