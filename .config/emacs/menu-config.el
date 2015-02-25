(provide 'menu-config)


;; package "helm" - аналог ido
(defun cfg:helm ()
  (require 'helm-config)
  (helm-mode t)
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  (setq helm-M-x-fuzzy-match t))
;; (add-hook 'cfg-hook:mode 'cfg:helm)

(defun cfg:ido-keys-hook ()
  (define-key ido-completion-map (kbd "M-k") 'ido-next-match)
  (define-key ido-completion-map (kbd "M-i") 'ido-prev-match)
  (define-key ido-completion-map (kbd "<up>") 'ido-prev-match)
  (define-key ido-completion-map (kbd "<down>") 'ido-next-match)
  (define-key ido-completion-map (kbd "<left>") 'ido-vertical-prev-match)
  (define-key ido-completion-map (kbd "<right>") 'ido-vertical-next-match))

(defun cfg:ido ()
  (require 'ido)
  (setq ido-everywhere t
	ido-use-filename-at-point nil
	ido-case-fold t
	ido-enable-flex-matching t
	ido-auto-merge-work-directories-length -1)
  (add-hook 'ido-setup-hook 'cfg:ido-keys-hook)
  (ido-mode t)
  (require 'ido-vertical-mode)
  (ido-vertical-mode t)
  (require 'smex)
  (smex-initialize))
(add-hook 'cfg-hook:mode 'cfg:ido)

;; -------------------- hooks --------------------

(defun menu-config-packages ()
  '(ido-vertical-mode smex))
;; ido-hacks, flx-ido, ido-ubiquitous - посмотреть
;; встроенный плагин bs возможно будет хорошим аналогом ido-switch-buffer

(defun menu-config-pre-load ())
(defun menu-config-load())
(defun menu-config-post-load ())
