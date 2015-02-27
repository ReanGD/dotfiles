(provide 'ui-config)


(defun cfg:screen ()
  (setq initial-scratch-message ""
	inhibit-startup-screen t
	inhibit-splash-screen t))
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

(defun lcl:set-mode-line-format ()
  (setq default-mode-line-format
	'("%e"
	  ;; buffer name
	  mode-line-buffer-identification
	  mode-line-client
	  mode-line-remote
	  mode-line-frame-identification
	  "   "
	  (vc-mode vc-mode)
	  "  "
	  mode-line-modes
	  mode-line-misc-info
	  ;; cursor position
	  "|" mode-line-front-space
	  ;; encode
	  mode-line-mule-info
	  ;; is modified and etc.
	  mode-line-modified
	  ;; not use
	  mode-line-position
	  ;; end spaces
	  mode-line-end-spaces))
  (setq mode-line-format default-mode-line-format))

(defun cfg:mode-line ()
  (lcl:set-mode-line-format)
  (setq sml/order-of-line-and-column nil
	sml/line-number-format "%3l"
	sml/numbers-separator ","
	sml/col-number-format "%2c"
	sml/position-percentage-format nil
	sml/mule-info "%z"
	sml/modified-char "*"
	sml/pre-minor-modes-separator " #"
	sml/pos-minor-modes-separator ""
	line-number-mode t
	column-number-mode t
	size-indication-mode nil)
  (require 'smart-mode-line)
  (setq sml/no-confirm-load-theme t)
  (sml/setup)
  (sml/apply-theme 'automatic))
(add-hook 'cfg-hook:ui 'cfg:mode-line)

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

;; -------------------- hooks --------------------

(defun ui-config-packages ()
  '(solarized-theme yascroll smart-mode-line))
;; smooth-scrolling, sublimity, smooth-scroll - не подходит
;; powerline powerline-evil - альтернатива smart-mode-line но вроде менее функциональная
;; rich-minority - изменение списка режимов

