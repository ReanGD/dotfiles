(provide 'hotkeys)


(defun cfg:hotkeys ()
  ;;(global-unset-key (kbd "M-z"))
  ;;(global-unset-key (kbd "C-x u"))
  (global-unset-key (kbd "C-x C-f"))
  ;;(global-unset-key (kbd "C-o"))
  (global-unset-key (kbd "C-x b"))
  ;;(global-unset-key (kbd "C-p"))
  ;;(global-unset-key (kbd "C-n"))
  ;;(global-unset-key (kbd "C-f"))
  (global-unset-key (kbd "C-b"))
  ;;(global-unset-key (kbd "M-x"))
  ;;(global-unset-key (kbd "M-X"))
  (global-unset-key (kbd "M-f"))
  (global-unset-key (kbd "M-b"))
  ;; (global-unset-key (kbd "C-a"))
  (global-unset-key (kbd "C-x h"))
  (global-unset-key (kbd "C-e"))
  (global-unset-key (kbd "M-w"))
  (global-unset-key (kbd "C-y"))
  
  ;; File
  (global-set-key (kbd "C-x u") 'undo-tree-visualize)
  (global-set-key (kbd "M-Z") 'undo-tree-redo)
  (global-set-key (kbd "M-z") 'undo-tree-undo)

  ;; Menu
  (global-set-key (kbd "C-o") 'cfg-func:find-file)       ;; C-x C-f
  (global-set-key (kbd "C-p") 'cfg-func:find-buffer)     ;; C-x b
  (global-set-key (kbd "M-x") 'cfg-func:find-command)
  (global-set-key (kbd "M-X") 'cfg-func:find-major-command)

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

  ;; Select
  (global-set-key (kbd "C-a")   'mark-whole-buffer)        ;; C-x h
  
  ;; Edit
  (global-set-key (kbd "C-S-d") 'md/duplicate-down)
  (global-set-key (kbd "C-S-c") 'kill-ring-save)         ;; M-w
  (global-set-key (kbd "C-S-v") 'yank)                   ;; C-y

  ;; Others
  (global-set-key (kbd "<f5>") 'cfg-func:compile)
  (if cfg-var:autocomplete
      (global-set-key (kbd "C-SPC") cfg-var:autocomplete))
  

  ;; Transform
  (global-unset-key (kbd "C-q"))
  (global-unset-key (kbd "M-c"))

  ;; Search
  (global-unset-key (kbd "C-s"))
  (global-unset-key (kbd "C-r"))

  ;; Search
  (global-set-key (kbd "C-f") 'isearch-forward)                          ;; C-s
  (define-key isearch-mode-map (kbd "<f3>") 'isearch-repeat-forward)     ;; C-s
  (define-key isearch-mode-map (kbd "S-<f3>") 'isearch-repeat-backward)  ;; C-r
  (define-key isearch-mode-map (kbd "<return>") 'isearch-repeat-forward) ;; C-s
  )
(add-hook 'cfg-hook:hotkey 'cfg:hotkeys)
