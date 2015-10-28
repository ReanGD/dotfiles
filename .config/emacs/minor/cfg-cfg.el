(provide 'minor/cfg-cfg)


(defvar cfg-mode-map (make-sparse-keymap))

(define-minor-mode cfg-mode
  "cfg-mode"
  :lighter " cfg"
  cfg-mode-map)

(defadvice load (after cfg-keybindings-priority)
  (if (not (eq (car (car minor-mode-map-alist)) 'cfg-mode))
      (let ((mykeys (assq 'cfg-mode minor-mode-map-alist)))
        (assq-delete-all 'cfg-mode minor-mode-map-alist)
        (add-to-list 'minor-mode-map-alist mykeys))))
(ad-activate 'load)

(defun turn-on-cfg-mode ()
  (interactive)
  (cfg-mode t))

(defun turn-off-cfg-mode ()
  (interactive)
  (cfg-mode -1))

(define-globalized-minor-mode global-cfg-mode cfg-mode turn-on-cfg-mode)

(defun lcl:get-hotkeys ()
  (list
   ;; File
   (list "C-s" 'save-buffer)              ;; C-x C-s
   (list "C-x u" 'undo-tree-visualize)
   (list "M-Z" 'undo-tree-redo)
   (list "M-z" 'undo-tree-undo)
   ;; Menu
   (list "C-o" 'cfg-func:find-file)       ;; C-x C-f
   (list "C-p" 'cfg-func:find-buffer)     ;; C-x b
   (list "C-S-p" 'cfg-func:find-command)  ;; M-x
   (list "M-P" 'cfg-func:find-major-command)
   (list "C-r" 'idomenu)
   (list "C-S-r" 'imenu-anywhere)
   ;; Move
   (list "M-i" 'previous-line)            ;; C-p
   (list "M-k" 'next-line)                ;; C-n
   (list "M-j" 'backward-char)            ;; C-b
   (list "M-l" 'forward-char)             ;; C-f
   (list "C-j" 'backward-word)            ;; M-b
   (list "C-l" 'forward-word)             ;; M-f
   (list "M-u" 'move-beginning-of-line)   ;; C-a
   (list "M-o" 'move-end-of-line)         ;; C-e
   (list "C-S-i" 'md/move-lines-up)
   (list "C-S-k" 'md/move-lines-down)
   (list "<prior>" 'cfg:page-up)          ;; prior/M-v
   (list "<next>" 'cfg:page-down)         ;; next/C-v
   (list "C-i" 'cfg:scroll-screen-up)
   (list "<C-up>" 'cfg:scroll-screen-up)
   (list "C-k" 'cfg:scroll-screen-down)
   (list "<C-down>" 'cfg:scroll-screen-down)
   ;; Select
   (list "C-a"   'mark-whole-buffer)      ;; C-x h
   ;; Edit
   (list "M-d" 'cfg:backward-delete-tab-whitespace)
   (list "<backspace>" 'cfg:backward-delete-tab-whitespace)
   (list "M-f" 'delete-char)              ;; C-d
   (list "<delete>" 'delete-char)
   (list "M-e" 'cfg:backward-delete-word)
   (list "<C-backspace>" 'cfg:backward-delete-word)
   (list "M-r" 'cfg:delete-word)          ;; M-d
   (list "<C-delete>" 'cfg:delete-word)
   (list "M-g" 'cfg:delete-line)          ;; C-k
   (list "C-S-d" 'md/duplicate-down)
   (list "C-S-c" 'kill-ring-save)         ;; M-w
   (list "M-c" 'kill-ring-save)           ;; M-w
   (list "C-S-v" 'yank)                   ;; C-y
   (list "M-v" 'yank)                     ;; C-y
   (list "C-S-x" 'kill-region)            ;; C-w
   (list "M-x" 'kill-region)              ;; C-w
   (list "C-/" 'comment-dwim-2)
   (list "<f12>" 'whitespace-mode)
   ;; Spelling
   (list "<f10>" 'cfg:langtool-check)
   (list "<C-f10>" 'langtool-correct-buffer)
   (list "<f11>" 'wcheck-mode)
   ;; Others
   (list "C-b" 'multi-compile-run)
   (if cfg-var:autocomplete
       (list "C-SPC" cfg-var:autocomplete))
   ;; My extensions
   (list "C-x C-f" 'cfg:format-buffer)
   ;; Search
   (list "C-f" 'swiper)          ;; C-s
   ))

(defun cfg:cfg-hotheys (map)
  (dolist (k (lcl:get-hotkeys))
    (when k
      (let ((key (kbd (car k)))
            (func (car (cdr k))))
        (define-key map key func)
        (global-set-key key func)
        ))))

(defun cfg:cfg ()
  (add-hook 'minibuffer-setup-hook 'turn-off-cfg-mode)
  (cfg:cfg-hotheys cfg-mode-map)
  (global-cfg-mode))

(add-hook 'cfg-hook:minor-mode 'cfg:cfg)
