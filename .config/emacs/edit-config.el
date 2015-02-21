(provide 'edit-config)

(defun edit-config-packages ()
  '(smartparens move-dup undo-tree))

;; autopair - менее функциональная замена smartparens


(defun session-settings ()
  (desktop-save-mode t)
  (setq initial-scratch-message ""
	inhibit-startup-screen t
	desktop-path '("~/.local/share/emacs/")
	desktop-dirname "~/.local/share/emacs/"
	desktop-enable t)
  (desktop-read)
  (delete-directory "~/.emacs.d"))


(defun autosave-settings ()
  ;;(setq make-backup-files         nil) ; Don't want any backup files
  ;;(setq auto-save-list-file-name  nil) ; Don't want any .saves files
  ;;(setq auto-save-default         nil) ; Don't want any auto saving
  )


(defun undo-settings ()
  (require 'undo-tree)
  (setq global-undo-tree-mode t
	undo-tree-auto-save-history t
	undo-tree-mode-lighter " Undo"
	undo-tree-history-directory-alist '((".*" . "~/.local/share/emacs/undo")))
  (add-hook 'ido-find-file-ex-hook 'undo-tree-load-history-hook)
  (global-unset-key (kbd "M-z"))
  (global-set-key (kbd "M-Z") 'undo-tree-redo)
  (global-set-key (kbd "M-z") 'undo-tree-undo))


(defun edit-settings ()
  ;; move with shift
  (setq shift-select-mode t)
  (put 'upcase-region 'disabled nil)
  (put 'downcase-region 'disabled nil)

  (setq show-paren-style 'expression)
  (show-paren-mode 2))


(defun ident-settings ()
  (electric-indent-mode +1))


(defun move-dup-settings ()
  (require 'move-dup)
  (global-set-key (kbd "C-S-d") 'md/duplicate-down)
  (global-set-key (kbd "C-S-i") 'md/move-lines-up)
  (global-set-key (kbd "C-S-k") 'md/move-lines-down))


(defun smartparens-settings()
  (require 'smartparens-config)
  (smartparens-global-mode t))


(defun edit-config-init ()
  (session-settings)
  (autosave-settings)
  (undo-settings)
  (edit-settings)
  (ident-settings)
  (move-dup-settings)
  (smartparens-settings))
