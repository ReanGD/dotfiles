(provide 'minor/ido-cfg)


(defun cfg:ido-keys-hook ()
  (define-key ido-completion-map (kbd "M-k") 'ido-next-match)
  (define-key ido-completion-map (kbd "M-i") 'ido-prev-match)
  (define-key ido-completion-map (kbd "<up>") 'ido-prev-match)
  (define-key ido-completion-map (kbd "<down>") 'ido-next-match)
  (define-key ido-completion-map (kbd "<left>") 'ido-vertical-prev-match)
  (define-key ido-completion-map (kbd "<right>") 'ido-vertical-next-match))
(add-hook 'ido-setup-hook 'cfg:ido-keys-hook)

(defun cfg:ido ()
  (require 'ido)
  (setq ido-everywhere t
	ido-use-filename-at-point nil
	ido-case-fold t
	ido-enable-flex-matching t
	ido-auto-merge-work-directories-length -1)
  (ido-mode t)
  (require 'ido-vertical-mode)
  (ido-vertical-mode t)
  (require 'smex)
  (smex-initialize))
(add-hook 'cfg-hook:minor-mode 'cfg:ido)

(cfg:add-package 'ido-vertical-mode)
(cfg:add-package 'smex)
;; ido-hacks, flx-ido, ido-ubiquitous - посмотреть
;; встроенный плагин bs возможно будет хорошим аналогом ido-switch-buffer
