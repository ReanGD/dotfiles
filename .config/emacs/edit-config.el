(provide 'edit-config)


(defun cfg:autosave ()
  ;;(setq make-backup-files         nil) ; Don't want any backup files
  ;;(setq auto-save-list-file-name  nil) ; Don't want any .saves files
  ;;(setq auto-save-default         nil) ; Don't want any auto saving
  )
(add-hook 'cfg-hook:ui 'cfg:autosave)

(defun cfg:undo ()
  (defvar undo-tree-map t)
  (require 'undo-tree)
  (global-undo-tree-mode)
  (setq undo-tree-auto-save-history nil
	undo-tree-mode-lighter " Undo"))
(add-hook 'cfg-hook:minor-mode 'cfg:undo)

(defun cfg:edit ()
  (delete-selection-mode t)
  (setq shift-select-mode t     ;; move with shift
	indent-tabs-mode nil    ;; отступы делаются пробелами
	tab-width 2             ;; default tab width
	c-basic-offset 2        ;; tab-width for cpp-lang (js, php, c++, java) can be eq tab-width
	js-indent-level 2       ;; indentation level in JS mode
	css-indent-offset 2     ;; indentation level in CSS mode
	)
  (put 'upcase-region 'disabled nil)
  (put 'downcase-region 'disabled nil)

  (electric-indent-mode +1)
  (setq show-paren-style 'expression)
  (show-paren-mode 2))
(add-hook 'cfg-hook:ui 'cfg:edit)

(defun cfg:move-dup ()
  (require 'move-dup))
(add-hook 'cfg-hook:minor-mode 'cfg:move-dup)

(defun cfg:smartparens ()
  (setq electric-pair-mode -1)
  (require 'smartparens-config)
  (smartparens-global-mode t))
(add-hook 'cfg-hook:minor-mode 'cfg:smartparens)

(cfg:add-package 'smartparens)
(cfg:add-package 'move-dup)
(cfg:add-package 'undo-tree)
;; autopair - менее функциональная замена smartparens
