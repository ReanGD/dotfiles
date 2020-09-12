(provide 'ui-config)


(defun cfg:screen ()
  (setq initial-scratch-message ""
        initial-major-mode 'text-mode
        inhibit-startup-screen t
        inhibit-splash-screen t)
  (add-to-list 'default-frame-alist
               '(font . "monospace 10")))
(add-hook 'cfg-hook:ui 'cfg:screen)

(defun cfg:bar ()
  ;; disable menu
  (menu-bar-mode -1)
  ;; disable tool-bar
  (tool-bar-mode -1))
(add-hook 'cfg-hook:ui 'cfg:bar)

(defun cfg:cursor ()
  ;; cursor |
  (set-default 'cursor-type 'bar)
  ;; select current line
  (global-hl-line-mode t)
  ;; disable beep
  (setq visible-bell t))
(add-hook 'cfg-hook:ui 'cfg:cursor)

(defun cfg:whitespace ()
  (setq whitespace-style '(face trailing lines empty space-mark tab-mark newline-mark)
        whitespace-line-column 100
        whitespace-display-mappings
        '((space-mark 32 [183] [46])
          (space-mark 160 [164] [95])
          (newline-mark 10 [182 10])
          (tab-mark 9 [187 9] [92 9])))
  (add-hook 'before-save-hook 'delete-trailing-whitespace))
(add-hook 'cfg-hook:ui 'cfg:whitespace)

(defun cfg:theme ()
  (setq x-underline-at-descent-line t
        solarized-distinct-fringe-background nil ;;t
        solarized-high-contrast-mode-line t
        solarized-use-less-bold nil
        solarized-use-more-italic t
        solarized-emphasize-indicators t
        solarized-scale-org-headlines nil)
  (if (equal cfg-var:theme "light")
      (load-theme 'solarized-light t))
  (if (equal cfg-var:theme "dark")
      (load-theme 'solarized-dark t)))
(add-hook 'cfg-hook:ui 'cfg:theme)

(defun cfg:scroll ()
  ;; smooth scroll settings
  (setq redisplay-dont-pause t
        auto-window-vscroll nil
        scroll-step 0
        scroll-margin 0
        scroll-conservatively most-positive-fixnum
        scroll-up-aggressively nil
        scroll-down-aggressively nil
        scroll-preserve-screen-position 'always)
  ;; disable scroll-bar
  (scroll-bar-mode -1)
  (global-yascroll-bar-mode 1)
  (setq yascroll:delay-to-hide nil)
  ;; Don't hide scrollbar when editing
  (defadvice yascroll:before-change (around always-show-bar activate) ()))
(add-hook 'cfg-hook:ui 'cfg:scroll)

(cfg:add-package 'solarized-theme)
(cfg:add-package 'yascroll)
;; smooth-scrolling, sublimity, smooth-scroll - не подходит
