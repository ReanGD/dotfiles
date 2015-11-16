(provide 'minor/search-cfg)


;; (defun lcl:seach-hotkeys ()
;;   (define-key isearch-mode-map (kbd "<f3>") 'isearch-repeat-forward)     ;; C-s
;;   (define-key isearch-mode-map (kbd "S-<f3>") 'isearch-repeat-backward)  ;; C-r
;;   (define-key isearch-mode-map (kbd "<return>") 'isearch-repeat-forward) ;; C-s
;;   )

(defun cfg:seach ()
  ;; (lcl:seach-hotkeys)
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-display-style 'fancy)
  )
(add-hook 'cfg-hook:minor-mode 'cfg:seach)

(cfg:add-package 'swiper)
