(provide 'sys/packages)

(require 'cl)

(require 'sys/groups)
(require 'sys/hooks)
(require 'sys/vars)
(require 'sys/funcs)
(require 'sys/remap)

(defun lcl:install-packages ()
  (let ((pkgs (remove-if (lambda (p) (package-installed-p (car p))) cfg-var:packages)))
    (when pkgs
      (message "%s" "Emacs is now refreshing its package database...")
      (package-refresh-contents)
      (message "%s" " done.")
      (dolist (p cfg-var:packages)
        (package-install (car p))
        (setq func (cdr p))
        (if func (funcall func))))))

(defun lcl:init-dir ()
  (setq delete-dir nil)

  (if (eq cfg-var:work-dir nil)
      (setq cfg-var:work-dir user-emacs-directory)
    (setq delete-dir user-emacs-directory))

  (setq backup-dir (concat (file-name-as-directory cfg-var:work-dir) "backup")
        auto-save-dir (concat (file-name-as-directory cfg-var:work-dir) "auto-save"))

  (setq user-emacs-directory cfg-var:work-dir
        backup-directory-alist `(("." . ,backup-dir))
        auto-save-list-file-prefix auto-save-dir)

  (if (not (eq delete-dir nil))
      (delete-directory delete-dir t)))

(defun lcl:init-session ()
  (desktop-save-mode t)
  (setq desktop-path (list cfg-var:work-dir)
        desktop-dirname cfg-var:work-dir
        desktop-enable t)
  (desktop-read))

(defun cfg:add-package (name &optional func)
  (add-to-list 'cfg-var:packages (cons name func)))

(defun cfg:pre-init ()
  (lcl:init-dir)
  (require 'package)
  (add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/") t)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
  (package-initialize)
  (setq-default flycheck-emacs-lisp-load-path load-path))

(defun cfg:post-init ()
  (lcl:install-packages)
  (run-hooks 'cfg-hook:ui)
  (run-hooks 'cfg-hook:minor-mode)
  (run-hooks 'cfg-hook:major-mode)
  (lcl:init-session)
  (run-hooks 'cfg-hook:session)
  (run-hooks 'cfg-hook:hotkey)
  (cfg:reverse-input-method 'russian-computer))
