(provide 'minor/mode-line-cfg)

(defface lcl-face:base
  '((t :inherit mode-line-inactive)) "")
(defface lcl-face:folder
  '((t :foreground "ForestGreen" :inherit lcl-face:base)) "")
(defface lcl-face:filename
  '((t :foreground "Blue" :weight bold :inherit lcl-face:base)) "")
(defface lcl-face:modifed
  '((t :foreground "Red" :inherit lcl-face:base)) "")
(defface lcl-face:major-mode
  '((t :weight bold :inherit lcl-face:base)) "")
(defface lcl-face:active
  '((t :weight bold :foreground "white" :background "#cda714" :inherit lcl-face:base)) "")
(defface lcl-face:inactive
  '((t :weight bold :foreground "white" :inherit mode-line)) "")

(defun lcl:ml-window-number ()
  (propertize (cfg:get-window-number)
              'face (if (eq lcl:selected-window (selected-window)) 'lcl-face:active 'lcl-face:inactive)))

(defun lcl:ml-position ()
  (propertize "%3l:%2c"
              'face 'lcl-face:base))

(defun lcl:ml-mule-info ()
  (propertize " %z"
              'help-echo 'mode-line-mule-info-help-echo
              'mouse-face 'mode-line-highlight
              'local-map mode-line-coding-system-map
              'face 'lcl-face:base))

(defun lcl:ml-modified-status ()
  (cond
   ((not (or (and (buffer-file-name) (file-remote-p buffer-file-name))
             (verify-visited-file-modtime (current-buffer))))
    (propertize "M"
                'face (if (eq lcl:selected-window (selected-window)) 'lcl-face:modifed 'lcl-face:base)
                'help-echo "Modified outside Emacs!\nRevert first!"))
   ((buffer-modified-p)
    (propertize (if buffer-read-only "R" "*")
                'face (if (eq lcl:selected-window (selected-window)) 'lcl-face:modifed 'lcl-face:base)
                'help-echo (if buffer-read-only "Read-Only Buffer" "Buffer Modified")))
   (buffer-read-only
    (propertize "R"
                'face (if (eq lcl:selected-window (selected-window)) 'lcl-face:modifed 'lcl-face:base)
                'help-echo "Read-Only Buffer"))
   (t
    (propertize " "
                'face 'lcl-face:base))
   ))

(defun lcl:ml-major-mode ()
  (propertize (concat " " mode-name)
              'face 'lcl-face:major-mode
              'help-echo "Major mode\n\
mouse-1: Display major mode menu\n\
mouse-2: Show help for major mode\n\
mouse-3: Toggle minor modes"
              'mouse-face 'mode-line-highlight
              'local-map mode-line-major-mode-keymap))

(defun lcl:ml-process ()
  (propertize (if mode-line-process ":%s" "")
              'face 'lcl-face:base))

(defun string/starts-with (string prefix)
  "Return t if STRING starts with prefix."
  (and (string-match (rx-to-string `(: bos ,prefix) t)
                     string)
       t))

(defun lcl:filer-minor-mode (mode)
  (if (or (string/starts-with mode "Projectile")
          (equal mode "cfg")  ;; local mode for hotkeys
          (equal mode "Anzu") ;; search-plugin
          )
      nil
    t))

(defun lcl:get-minor-modes ()
  (remove-if-not
   (lambda (mode) (lcl:filer-minor-mode mode))
   (split-string (format-mode-line minor-mode-alist))))

(defun lcl:get-minor-mode-display-name (mode)
  (if (equal mode "yas") ;; YASnippet
      "YAS"
    (if (equal mode "company") ;; autocomplete Company-mode
        "CO"
      (if (equal mode "SP") ;; smartparens mode
          "(S)"
        mode
        ))))

(defun lcl:ml-mouse-event (click-type string)
  (cond ((eq click-type 'menu)
         `(lambda (event)
            (interactive "@e")
            (minor-mode-menu-from-indicator ,string)))
        ((eq click-type 'help)
         `(lambda (event)
            (interactive "@e")
            (describe-minor-mode-from-indicator ,string)))))

(defun lcl:ml-minor-mode-map (mode-name)
  (let ((map (make-sparse-keymap)))
    (define-key map
      [mode-line down-mouse-1]
      (lcl:ml-mouse-event 'menu mode-name))
    (define-key map
      [mode-line mouse-2]
      (lcl:ml-mouse-event 'help mode-name))
    (define-key map
      [mode-line down-mouse-3]
      (lcl:ml-mouse-event 'menu mode-name))
    (define-key map
      [header-line down-mouse-3]
      (lcl:ml-mouse-event 'menu mode-name))
    map))

(defun lcl:ml-minor-modes (&optional face pad)
  (mapconcat (lambda (mm)
               (concat (propertize " " 'face 'lcl-face:base)
                       (propertize (lcl:get-minor-mode-display-name mm)
                                   'face 'lcl-face:base
                                   'mouse-face 'mode-line-highlight
                                   'help-echo "Minor mode\n mouse-1: Display minor mode menu\n mouse-2: Show help for minor mode\n mouse-3: Toggle minor modes"
                                   'local-map (lcl:ml-minor-mode-map mm)
                                   )))
             (lcl:get-minor-modes) ""))

(defvar lcl:buffer-id nil)
(make-variable-buffer-local 'lcl:buffer-id)
(put 'lcl:buffer-id 'risky-local-variable t)
(defvar lcl:projectile-loaded-p nil)
(defvar lcl:mode-line-buffer-identification
  '("" (lcl:buffer-id
        lcl:buffer-id
        (:eval (lcl:fill-buffer-id)))))

(declare-function projectile-project-p "projectile")
(declare-function projectile-project-name "projectile")

(defun lcl:get-buffer-id-projectile (proj-directory)
  (concat "[" (projectile-project-name) "]"
          (replace-regexp-in-string (concat proj-directory "*") "" default-directory)))

(defun lcl:get-buffer-id-line (proj-directory)
  (if (stringp buffer-file-name)
      (if (stringp proj-directory)
          (lcl:get-buffer-id-projectile proj-directory)
        default-directory)
    ""))

(defun lcl:get-buffer-id-line-help (proj-directory)
  (concat
   (if (stringp buffer-file-name)
       (if (stringp proj-directory)
           (format "Project: %s (%s)\nFile: %s" (projectile-project-name) proj-directory buffer-file-name)
         (format "File: %s" buffer-file-name))
     (buffer-name))
   "\n\nmouse-1: Previous buffer\nmouse-3: Next buffer"))

(defun lcl:fill-buffer-id (&rest ignored)
  (setq lcl:buffer-id
        (let ((proj-directory (if lcl:projectile-loaded-p (projectile-project-p) nil)))
          (propertize
           (concat (propertize (lcl:get-buffer-id-line proj-directory)
                               'face 'lcl-face:folder)
                   (propertize (buffer-name)
                               'face 'lcl-face:filename))
           'help-echo (lcl:get-buffer-id-line-help proj-directory)
           'local-map mode-line-buffer-identification-keymap))))

(defun lcl:set-buffer-id (&rest ignored)
  (setq mode-line-buffer-identification lcl:mode-line-buffer-identification))

(defadvice rename-buffer (after lcl:after-rename-buffer ())
  (lcl:fill-buffer-id))

(defadvice set-visited-file-name (after lcl:after-set-visited-file-name ())
  (lcl:fill-buffer-id))

(add-hook 'after-save-hook 'lcl:fill-buffer-id)
(add-hook 'clone-indirect-buffer-hook 'lcl:fill-buffer-id)
(add-hook 'comint-output-filter-functions 'lcl:fill-buffer-id)
(add-hook 'eshell-directory-change-hook 'lcl:fill-buffer-id)
(add-hook 'dired-mode-hook 'lcl:set-buffer-id)
(eval-after-load "projectile" '(setq lcl:projectile-loaded-p t))

(eval-after-load "vc-hooks" 
  '(defadvice vc-mode-line (after sml/after-vc-mode-line-advice () activate)
     (when (stringp vc-mode)
       (let ((backend (replace-regexp-in-string (format "^ %s" (vc-backend buffer-file-name)) " " vc-mode)))
         (setq vc-mode
               (propertize backend
                           'face (cond ((string-match "^ -"      backend) 'lcl-face:base) ;; sml/vc
                                       ((string-match "^ [:@]"   backend) 'lcl-face:base) ;; sml/vc-edited)
                                       ((string-match "^ [!\\?]" backend) 'lcl-face:base) ;; sml/modified)
                                       )))))))


(defvar lcl:selected-window (frame-selected-window))
(defun lcl:set-selected-window ()
  "sets the variable `powerline-selected-window` appropriately"
  (when (not (minibuffer-window-active-p (frame-selected-window)))
    (setq lcl:selected-window (frame-selected-window))))

(add-hook 'window-configuration-change-hook 'lcl:set-selected-window)
(add-hook 'focus-in-hook 'lcl:set-selected-window)
(add-hook 'focus-out-hook 'lcl:set-selected-window)

(defadvice select-window (after lcl:select-window activate)
  "makes powerline aware of window changes"
  (lcl:set-selected-window))


(defun lcl:mode-line-left ()
  '((:eval (lcl:ml-window-number))
    (:eval (lcl:ml-modified-status))
    mode-line-buffer-identification
    (:eval (lcl:ml-major-mode))
    (:eval (lcl:ml-process))
    (:eval (lcl:ml-minor-modes))
    (vc-mode vc-mode)))

(defun lcl:mode-line-right ()
  '((:eval (lcl:ml-position))
    (:eval (lcl:ml-mule-info))
    (:propertize "  " face lcl-face:base)))

(defun lcl:mode-line-spaces (cnt)
  (let ((spaces (format (format "%%%ds" cnt) "")))
    (format-mode-line '((:propertize spaces face lcl-face:base)))))

(defun simple-mode-line-render (left right)
  (let* ((available-width (- (window-width) (length left) (length right) -3)))
    (format "%s%s%s" left (lcl:mode-line-spaces available-width) right)
    ))

(defun cfg:mode-line ()
  ;; (eq lcl:selected-window (selected-window))
  (setq-default mode-line-buffer-identification lcl:mode-line-buffer-identification)
  (setq-default mode-line-format
                '((:eval (simple-mode-line-render
                          (format-mode-line (lcl:mode-line-left))
                          (format-mode-line (lcl:mode-line-right))))))
  )
(add-hook 'cfg-hook:ui 'cfg:mode-line)
