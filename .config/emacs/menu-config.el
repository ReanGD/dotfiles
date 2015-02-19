(provide 'menu-config)

(defun menu-config-packages ()
  '(ido-vertical-mode ido-ubiquitous smex))

;; helm - аналог ido
;; ido-hacks, flx-ido - посмотреть
;; встроенный плагин bs возможно будет хорошим аналогом ido-switch-buffer

(defun helm-config()
  (require 'helm-config)
  (helm-mode t)
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  (setq helm-M-x-fuzzy-match t)
  )


(defun ido-match-keys ()
  (define-key ido-completion-map (kbd "M-k") 'ido-next-match)
  (define-key ido-completion-map (kbd "M-i") 'ido-prev-match)
  (define-key ido-completion-map (kbd "<up>") 'ido-prev-match)
  (define-key ido-completion-map (kbd "<down>") 'ido-next-match)
  (define-key ido-completion-map (kbd "<left>") 'ido-vertical-prev-match)
  (define-key ido-completion-map (kbd "<right>") 'ido-vertical-next-match))


(defun ido-keys ()
  (global-set-key (kbd "C-o") 'ido-find-file)       ;; C-x C-f
  (global-unset-key (kbd "C-p"))
  (global-set-key (kbd "C-p") 'ido-switch-buffer)   ;; C-x b
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands))


(defun ido-config()
  (require 'ido)
  (ido-mode t)
  (require 'ido-vertical-mode)
  (ido-vertical-mode t)
  (require 'smex)
  (smex-initialize)
  (add-hook 'ido-setup-hook 'ido-match-keys)
  (ido-keys)
  (setq ido-everywhere t
	ido-use-filename-at-point nil
	ido-case-fold t
	ido-enable-flex-matching t
	ido-auto-merge-work-directories-length -1)
  )


(defun menu-config-init()
  ;;(helm-config)
  (ido-config)
  )
