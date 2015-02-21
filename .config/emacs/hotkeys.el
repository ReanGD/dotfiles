(provide 'hotkeys)


(defun hotkeys-settings ()
  ;; File
  (global-unset-key (kbd "M-z"))
  (global-unset-key (kbd "C-x u"))
  (global-set-key (kbd "C-x u") 'undo-tree-visualize)
  (global-set-key (kbd "M-Z") 'undo-tree-redo)
  (global-set-key (kbd "M-z") 'undo-tree-undo)

  ;; Move
  (global-unset-key (kbd "C-f"))
  (global-unset-key (kbd "C-n"))
  (global-unset-key (kbd "C-b"))
  (global-unset-key (kbd "M-f"))
  (global-unset-key (kbd "M-b"))
  (global-unset-key (kbd "C-a"))
  (global-unset-key (kbd "C-e"))
  (global-unset-key (kbd "C-x h"))

  ;; Transform
  (global-unset-key (kbd "M-w"))
  (global-unset-key (kbd "C-y"))
  (global-unset-key (kbd "C-q"))
  (global-unset-key (kbd "M-c"))

  ;; Search
  (global-unset-key (kbd "C-s"))
  (global-unset-key (kbd "C-r"))
  (global-unset-key (kbd "C-x b"))

  ;; File
  (global-unset-key (kbd "C-x C-f"))
  
  ;; Move
  (global-set-key (kbd "M-l") 'forward-char)             ;; C-f
  (global-set-key (kbd "M-k") 'next-line)                ;; C-n
  (global-set-key (kbd "M-j") 'backward-char)            ;; C-b
  (global-set-key (kbd "M-i") 'previous-line)            ;; C-p
  (global-set-key (kbd "C-l") 'forward-word)             ;; M-f
  (global-set-key (kbd "C-j") 'backward-word)            ;; M-b
  (global-set-key (kbd "M-u") 'move-beginning-of-line)   ;; C-a
  (global-set-key (kbd "M-o") 'move-end-of-line)         ;; C-e
  (global-set-key (kbd "C-a") 'mark-whole-buffer)        ;; C-x h

  ;; Cursor
  (global-set-key (kbd "C-S-l") 'mc/edit-lines)

  ;; Transform
  (global-set-key (kbd "C-S-c") 'kill-ring-save)         ;; M-w
  (global-set-key (kbd "C-S-v") 'yank)                   ;; C-y

  ;; Search
  (global-set-key (kbd "C-f") 'isearch-forward)                          ;; C-s
  (define-key isearch-mode-map (kbd "<f3>") 'isearch-repeat-forward)     ;; C-s
  (define-key isearch-mode-map (kbd "S-<f3>") 'isearch-repeat-backward)  ;; C-r
  (define-key isearch-mode-map (kbd "<return>") 'isearch-repeat-forward) ;; C-s
  )

;; -------------------- hooks --------------------

(defun hotkeys-packages()
  '())

(defun hotkeys-on-load ()
  )

(defun hotkeys-on-configure ()
  (hotkeys-settings))

(defun hotkeys-on-exit ()
  )
