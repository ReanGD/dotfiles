(provide 'minor/search-cfg)


(defun lcl:seach-hotkeys ()
  (define-key ivy-minibuffer-map (kbd "M-i") 'ivy-previous-line)
  (define-key ivy-minibuffer-map (kbd "M-k") 'ivy-next-line)
  )

(defun cfg:seach ()
  (ivy-mode 1)
  (lcl:seach-hotkeys)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-display-style 'fancy)
  )
(add-hook 'cfg-hook:minor-mode 'cfg:seach)

(cfg:add-package 'swiper)
