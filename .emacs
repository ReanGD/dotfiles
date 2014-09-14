(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
;; Построчная прокрутка
(setq scroll-step 1)
;; select current line
(global-hl-line-mode 1)
;; cursor |
(set-default 'cursor-type 'bar)
;; session save
(desktop-save-mode t)
;; move wit shift
(setq shift-select-mode t)
;; disable menu
(menu-bar-mode -1)
;; disable tool-bar
(tool-bar-mode -1)
;; disable beep
(setq visible-bell t)
;; auto-ident
(electric-indent-mode +1)

;;(setq make-backup-files         nil) ; Don't want any backup files
;;(setq auto-save-list-file-name  nil) ; Don't want any .saves files
;;(setq auto-save-default         nil) ; Don't want any auto saving
(setq show-paren-style 'expression)
(show-paren-mode 2)

(add-to-list 'load-path "~/.emacs.d/")

;; (setq url-proxy-services '(("http" . "172.16.1.130:8080")))

(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
  )

(require 'cl)
 
(defvar my-packages
  '(auto-complete python-environment autopair flycheck))
 
(defun my-packages-installed-p ()
  (loop for p in my-packages
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))
 
(unless (my-packages-installed-p)
  (package-refresh-contents)
  (dolist (p my-packages)
    (when (not (package-installed-p p))
      (package-install p))))

(if (not (package-installed-p 'jedi))
   (progn
     (package-refresh-contents)
     (package-install 'jedi)
     (setq jedi:environment-virtualenv
       (list "virtualenv2" "--system-site-packages"))
     (jedi:install-server)))


;; built-in
(require 'ido)
(ido-mode t)
(ido-everywhere 1)
(setq ido-use-filename-at-point nil)
(setq ido-case-fold t)
(setq ido-enable-flex-matching t)
(setq ido-auto-merge-work-directories-length -1)

(require 'bs)
(setq bs-configurations
      '(("files" "^\\*scratch\\*" nil nil bs-visits-non-file bs-sort-buffer-interns-are-last)))

;; jedi
(setq jedi:environment-virtualenv
  (list "virtualenv2" "--system-site-packages"))
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)

(add-hook 'after-init-hook #'global-flycheck-mode)

(require 'autopair)
(autopair-global-mode) 

;;(require 'projectile)
;;(projectile-global-mode)

;; (require 'auto-complete-config)
;; (ac-config-default)


;; http://www.emacswiki.org/emacs/AutoComplete
;;(add-to-list 'load-path "~/EmacsCasts/episode03/auto-complete")
;;(require 'auto-complete-config)
;;(ac-config-default)
;;(add-to-list 'ac-dictionary-directories "/home/dim/EmacsCasts/episode03/auto-complete/dict")


;; http://www.emacswiki.org/emacs/SrSpeedbar
;;(require 'sr-speedbar)
;;(global-set-key (kbd "<f12>") 'sr-speedbar-toggle)


;; http://www.emacswiki.org/emacs/Yasnippet
;;(add-to-list 'load-path "~/EmacsCasts/episode03/yasnippet")
;;(require 'yasnippet)
;;(yas-global-mode 1)
;;(yas/load-directory "~/EmacsCasts/episode03/yasnippet/snippets")

;; Options -> Set default font
;;(add-to-list 'default-frame-alist '(font . "Consolas-18"))
;;(set-default-font "Consolas-18")

;; installed packages
;; solarized-emacs
;; multiple-cursors
;; move-text
;; M-x load theme и можно установить темную тему
;; (require 'multiple-cursors)


;; functions
(defun reverse-input-method (input-method)
  "Build the reverse mapping of single letters from INPUT-METHOD."
  (interactive
   (list (read-input-method-name "Use input method (default current): ")))
  (if (and input-method (symbolp input-method))
      (setq input-method (symbol-name input-method)))
  (let ((current current-input-method)
  (modifiers '(nil (control) (meta) (control meta))))
    (when input-method
      (activate-input-method input-method))
    (when (and current-input-method quail-keyboard-layout)
      (dolist (map (cdr (quail-map)))
  (let* ((to (car map))
         (from (quail-get-translation
          (cadr map) (char-to-string to) 1)))
    (when (and (characterp from) (characterp to))
      (dolist (mod modifiers)
        (define-key local-function-key-map
    (vector (append mod (list from)))
    (vector (append mod (list to)))))))))
    (when input-method
      (activate-input-method current))))

(reverse-input-method 'russian-computer)

(defun duplicate-line (arg)
  "Duplicate current line, leaving point in lower line."
  (interactive "*p")
  ;; save the point for undo
  (setq buffer-undo-list (cons (point) buffer-undo-list))
  ;; local variables for start and end of line
  (let ((bol (save-excursion (beginning-of-line) (point)))
        eol)
    (save-excursion
      ;; don't use forward-line for this, because you would have
      ;; to check whether you are at the end of the buffer
      (end-of-line)
      (setq eol (point))
      ;; store the line and disable the recording of undo information
      (let ((line (buffer-substring bol eol))
            (buffer-undo-list t)
            (count arg))
        ;; insert the line arg times
        (while (> count 0)
          (newline)         ;; because there is no newline in 'line'
          (insert line)
          (setq count (1- count)))
        )
      ;; create the undo information
      (setq buffer-undo-list (cons (cons eol (point)) buffer-undo-list)))
    ) ; end-of-let
  ;; put the point in the lowest line and return
  (next-line arg))

(defun move-line-up ()
  (interactive)
  (transpose-lines 1)
  (forward-line -2))

(defun move-line-down ()
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1))

;; Move
(global-unset-key (kbd "C-f"))
(global-unset-key (kbd "C-n"))
(global-unset-key (kbd "C-b"))
(global-unset-key (kbd "C-p"))
(global-unset-key (kbd "M-f"))
(global-unset-key (kbd "M-b"))
(global-unset-key (kbd "C-a"))
(global-unset-key (kbd "C-e"))
(global-unset-key (kbd "C-x h"))

;; Transform
(global-unset-key (kbd "M-w"))
(global-unset-key (kbd "C-y"))

;; Search
(global-unset-key (kbd "C-s"))
(global-unset-key (kbd "C-r"))

;; File
(global-unset-key (kbd "C-x C-f"))
(global-unset-key (kbd "C-x C-s"))


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
(global-set-key (kbd "C-S-d") 'duplicate-line)
(global-set-key (kbd "C-S-i") 'move-line-up)
(global-set-key (kbd "C-S-k") 'move-line-down)

;; Cursor
(global-set-key (kbd "C-S-l") 'mc/edit-lines)

;; Transform
(global-set-key (kbd "C-S-c") 'kill-ring-save)         ;; M-w
(global-set-key (kbd "C-S-v") 'yank)                   ;; C-y

;; Search
(global-set-key (kbd "C-f") 'isearch-forward)          ;; C-s
(define-key isearch-mode-map (kbd "<f3>") 'isearch-repeat-forward)     ;; C-s
(define-key isearch-mode-map (kbd "S-<f3>") 'isearch-repeat-backward)  ;; C-r
(define-key isearch-mode-map (kbd "<return>") 'isearch-repeat-forward) ;; C-s

;; File
(global-set-key (kbd "C-o") 'ido-find-file)            ;; C-x C-f
(global-set-key (kbd "C-s") 'save-buffer)              ;; C-x C-s
;;(global-set-key (kbd "C-p") 'bs-show)
