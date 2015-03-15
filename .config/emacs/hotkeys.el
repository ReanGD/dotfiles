(provide 'hotkeys)


(defun cfg:hotkeys ()
  ;;(global-unset-key (kbd "C-x u"))
  ;;(global-unset-key (kbd "C-x C-f"))
  (global-unset-key (kbd "C-x C-s"))
  (global-unset-key (kbd "C-x h"))
  (global-unset-key (kbd "C-x b"))
  ;;(global-unset-key (kbd "C-a"))
  ;;(global-unset-key (kbd "C-b"))
  ;;(global-unset-key (kbd "C-n"))
  ;;(global-unset-key (kbd "C-f"))
  ;;(global-unset-key (kbd "C-o"))
  ;;(global-unset-key (kbd "C-p"))
  ;;(global-unset-key (kbd "C-s"))
  ;;(global-unset-key (kbd "C-v"))
  ;;(global-unset-key (kbd "C-/"))
  (global-unset-key (kbd "C-e"))
  (global-unset-key (kbd "C-k"))
  (global-unset-key (kbd "C-d"))
  (global-unset-key (kbd "C-q"))
  ;;(global-unset-key (kbd "C-r"))
  (global-unset-key (kbd "C-y"))
  (global-unset-key (kbd "C-z"))
  ;;(global-unset-key (kbd "M-z"))
  ;;(global-unset-key (kbd "M-v"))
  ;;(global-unset-key (kbd "M-f"))
  ;;(global-unset-key (kbd "M-d"))
  ;;(global-unset-key (kbd "M-e"))
  ;;(global-unset-key (kbd "M-r"))
  ;;(global-unset-key (kbd "M-g"))
  (global-unset-key (kbd "M-c"))
  (global-unset-key (kbd "M-x"))
  (global-unset-key (kbd "M-X"))
  (global-unset-key (kbd "M-b"))
  (global-unset-key (kbd "M-w"))
  ;;(global-unset-key (kbd "<prior>"))
  ;;(global-unset-key (kbd "<next>"))
  ;;(global-unset-key (kbd "<delete>"))
  ;;(global-unset-key (kbd "<backspace>"))
  ;;(global-unset-key (kbd "<C-delete>"))
  ;;(global-unset-key (kbd "<C-backspace>"))
  
  ;; File
  (global-set-key (kbd "C-s") 'save-buffer)              ;; C-x C-s
  (global-set-key (kbd "C-x u") 'undo-tree-visualize)
  (global-set-key (kbd "M-Z") 'undo-tree-redo)
  (global-set-key (kbd "M-z") 'undo-tree-undo)

  ;; Menu
  (global-set-key (kbd "C-o") 'cfg-func:find-file)       ;; C-x C-f
  (global-set-key (kbd "C-p") 'cfg-func:find-buffer)     ;; C-x b
  (global-set-key (kbd "C-S-p") 'cfg-func:find-command)  ;; M-x
  (global-set-key (kbd "M-P") 'cfg-func:find-major-command)
  (global-set-key (kbd "C-r") 'idomenu)
  (global-set-key (kbd "C-S-r") 'imenu-anywhere)

  ;; Move
  (global-set-key (kbd "M-i") 'previous-line)            ;; C-p
  (global-set-key (kbd "M-k") 'next-line)                ;; C-n
  (global-set-key (kbd "M-j") 'backward-char)            ;; C-b
  (global-set-key (kbd "M-l") 'forward-char)             ;; C-f
  (global-set-key (kbd "C-j") 'backward-word)            ;; M-b
  (global-set-key (kbd "C-l") 'forward-word)             ;; M-f
  (global-set-key (kbd "M-u") 'move-beginning-of-line)   ;; C-a
  (global-set-key (kbd "M-o") 'move-end-of-line)         ;; C-e
  (global-set-key (kbd "C-S-i") 'md/move-lines-up)
  (global-set-key (kbd "C-S-k") 'md/move-lines-down)
  (global-set-key (kbd "M-v") 'cfg:scroll-up)            ;; M-v
  (global-set-key (kbd "<next>") 'cfg:scroll-up)         ;; next
  (global-set-key (kbd "C-v") 'cfg:scroll-down)          ;; C-v
  (global-set-key (kbd "<prior>") 'cfg:scroll-down)      ;; prior

  ;; Select
  (global-set-key (kbd "C-a")   'mark-whole-buffer)      ;; C-x h

  ;; Edit
  (global-set-key (kbd "M-d") 'cfg:backspace-soft-tab-once)
  (global-set-key (kbd "<backspace>") 'cfg:backspace-soft-tab-once)
  (global-set-key (kbd "M-f") 'delete-char)              ;; C-d
  (global-set-key (kbd "<delete>") 'delete-char)
  (global-set-key (kbd "M-e") 'cfg:backward-delete-word)
  (global-set-key (kbd "<C-backspace>") 'cfg:backward-delete-word)
  (global-set-key (kbd "M-r") 'cfg:delete-word)          ;; M-d
  (global-set-key (kbd "<C-delete>") 'cfg:delete-word)
  (global-set-key (kbd "M-g") 'cfg:delete-line)          ;; C-k
  (global-set-key (kbd "C-S-d") 'md/duplicate-down)
  (global-set-key (kbd "C-S-c") 'kill-ring-save)         ;; M-w
  (global-set-key (kbd "C-S-v") 'yank)                   ;; C-y
  (global-set-key (kbd "C-/")   'comment-dwim-2)
  (global-set-key (kbd "<f12>") 'whitespace-mode)

  ;; Others
  (global-set-key (kbd "C-b") 'cfg-func:compile)
  (if cfg-var:autocomplete
      (global-set-key (kbd "C-SPC") cfg-var:autocomplete))

  ;; My extensions
  (global-set-key (kbd "C-x C-f") 'cfg:format-buffer)
  

  
  ;; Search
  (global-set-key (kbd "C-f") 'isearch-forward)                          ;; C-s
  (define-key isearch-mode-map (kbd "<f3>") 'isearch-repeat-forward)     ;; C-s
  (define-key isearch-mode-map (kbd "S-<f3>") 'isearch-repeat-backward)  ;; C-r
  (define-key isearch-mode-map (kbd "<return>") 'isearch-repeat-forward) ;; C-s
  )
(add-hook 'cfg-hook:hotkey 'cfg:hotkeys)
