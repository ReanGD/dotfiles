(provide 'minor/search-cfg)


(defun lcl:seach-hotkeys ()
  (define-key isearch-mode-map (kbd "<f3>") 'isearch-repeat-forward)     ;; C-s
  (define-key isearch-mode-map (kbd "S-<f3>") 'isearch-repeat-backward)  ;; C-r
  (define-key isearch-mode-map (kbd "<return>") 'isearch-repeat-forward) ;; C-s
  )

(defun cfg:seach ()
  (global-anzu-mode +1)
  (lcl:seach-hotkeys)
  )
(add-hook 'cfg-hook:minor-mode 'cfg:seach)

(cfg:add-package 'anzu)
;; (cfg:add-package 'isearch+)
