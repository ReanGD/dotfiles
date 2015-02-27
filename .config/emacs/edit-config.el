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
  (setq shift-select-mode t     ;; move with shift
	delete-selection-mode t ;; delete selection
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

;; -------------------- hooks --------------------

(defun edit-config-packages ()
  '(smartparens move-dup undo-tree))
;; autopair - менее функциональная замена smartparens
