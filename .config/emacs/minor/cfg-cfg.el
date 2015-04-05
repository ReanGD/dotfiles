(provide 'minor/cfg-cfg)


(defvar cfg-mode-map (make-sparse-keymap))

(define-minor-mode cfg-mode
  "cfg-mode"
  :lighter " cfg"
  cfg-mode-map)

(defadvice load (after give-my-keybindings-priority)
  "Try to ensure that my keybindings always have priority."
  (if (not (eq (car (car minor-mode-map-alist)) 'cfg-mode))
      (let ((mykeys (assq 'cfg-mode minor-mode-map-alist)))
        (assq-delete-all 'cfg-mode minor-mode-map-alist)
        (add-to-list 'minor-mode-map-alist mykeys))))
(ad-activate 'load)

(defun turn-on-cfg-mode ()
  "Turns on my-mode."
  (interactive)
  (cfg-mode t))

(defun turn-off-cfg-mode ()
  "Turns off my-mode."
  (interactive)
  (cfg-mode -1))

(define-globalized-minor-mode global-cfg-mode cfg-mode turn-on-cfg-mode)

(defun cfg:cfg-hotheys (map)
  ;; File
  (define-key map (kbd "C-s") 'save-buffer)              ;; C-x C-s
  (define-key map (kbd "C-x u") 'undo-tree-visualize)
  (define-key map (kbd "M-Z") 'undo-tree-redo)
  (define-key map (kbd "M-z") 'undo-tree-undo)

  ;; Menu
  (define-key map (kbd "C-o") 'cfg-func:find-file)       ;; C-x C-f
  (define-key map (kbd "C-p") 'cfg-func:find-buffer)     ;; C-x b
  (define-key map (kbd "C-S-p") 'cfg-func:find-command)  ;; M-x
  (define-key map (kbd "M-P") 'cfg-func:find-major-command)
  (define-key map (kbd "C-r") 'idomenu)
  (define-key map (kbd "C-S-r") 'imenu-anywhere)

  ;; Move
  (define-key map (kbd "M-i") 'previous-line)            ;; C-p
  (define-key map (kbd "M-k") 'next-line)                ;; C-n
  (define-key map (kbd "M-j") 'backward-char)            ;; C-b
  (define-key map (kbd "M-l") 'forward-char)             ;; C-f
  (define-key map (kbd "C-j") 'backward-word)            ;; M-b
  (define-key map (kbd "C-l") 'forward-word)             ;; M-f
  (define-key map (kbd "M-u") 'move-beginning-of-line)   ;; C-a
  (define-key map (kbd "M-o") 'move-end-of-line)         ;; C-e
  (define-key map (kbd "C-S-i") 'md/move-lines-up)
  (define-key map (kbd "C-S-k") 'md/move-lines-down)
  (define-key map (kbd "<prior>") 'cfg:page-up)          ;; prior/M-v
  (define-key map (kbd "<next>") 'cfg:page-down)         ;; next/C-v
  (define-key map (kbd "C-i") 'cfg:scroll-screen-up)
  (define-key map (kbd "<C-up>") 'cfg:scroll-screen-up)
  (define-key map (kbd "C-k") 'cfg:scroll-screen-down)
  (define-key map (kbd "<C-down>") 'cfg:scroll-screen-down)
  
  ;; Select
  (define-key map (kbd "C-a")   'mark-whole-buffer)      ;; C-x h

  ;; Edit
  (define-key map (kbd "M-d") 'cfg:backspace-soft-tab-once)
  (define-key map (kbd "<backspace>") 'cfg:backspace-soft-tab-once)
  (define-key map (kbd "M-f") 'delete-char)              ;; C-d
  (define-key map (kbd "<delete>") 'delete-char)
  (define-key map (kbd "M-e") 'cfg:backward-delete-word)
  (define-key map (kbd "<C-backspace>") 'cfg:backward-delete-word)
  (define-key map (kbd "M-r") 'cfg:delete-word)          ;; M-d
  (define-key map (kbd "<C-delete>") 'cfg:delete-word)
  (define-key map (kbd "M-g") 'cfg:delete-line)          ;; C-k
  (define-key map (kbd "C-S-d") 'md/duplicate-down)
  (define-key map (kbd "C-S-c") 'kill-ring-save)         ;; M-w
  (define-key map (kbd "C-S-v") 'yank)                   ;; C-y
  (define-key map (kbd "C-/")   'comment-dwim-2)
  (define-key map (kbd "<f12>") 'whitespace-mode)

  ;; Others
  (define-key map (kbd "C-b") 'cfg-func:compile)
  (if cfg-var:autocomplete
      (define-key map (kbd "C-SPC") cfg-var:autocomplete))

  ;; My extensions
  (define-key map (kbd "C-x C-f") 'cfg:format-buffer)

  ;; Search
  (define-key map (kbd "C-f") 'isearch-forward)          ;; C-s
  )

(defun cfg:cfg ()
  (add-hook 'minibuffer-setup-hook 'turn-off-cfg-mode)
  (cfg:cfg-hotheys cfg-mode-map)
  (global-cfg-mode)
  )

(add-hook 'cfg-hook:minor-mode 'cfg:cfg)
