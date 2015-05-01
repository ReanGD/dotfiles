(provide 'hotkeys)


(defun cfg:hotkeys ()
  ;;(global-unset-key (kbd "C-x u"))
  ;;(global-unset-key (kbd "C-x C-f"))
  (global-unset-key (kbd "C-x C-s"))
  (global-unset-key (kbd "C-x h"))
  (global-unset-key (kbd "C-x b"))
  ;;(global-unset-key (kbd "C-a"))
  ;;(global-unset-key (kbd "C-b"))
  ;;(global-unset-key (kbd "C-n"))
  ;;(global-unset-key (kbd "C-f"))
  ;;(global-unset-key (kbd "C-o"))
  ;;(global-unset-key (kbd "C-p"))
  ;;(global-unset-key (kbd "C-s"))
  (global-unset-key (kbd "C-v"))
  ;;(global-unset-key (kbd "C-/"))
  ;;(global-unset-key (kbd "C-k"))
  ;;(global-unset-key (kbd "C-i"))
  ;;(global-unset-key (kbd "C-r"))
  (global-unset-key (kbd "C-w"))
  (global-unset-key (kbd "C-e"))
  (global-unset-key (kbd "C-d"))
  (global-unset-key (kbd "C-q"))
  (global-unset-key (kbd "C-y"))
  (global-unset-key (kbd "C-z"))
  ;;(global-unset-key (kbd "M-z"))
  ;;(global-unset-key (kbd "M-f"))
  ;;(global-unset-key (kbd "M-d"))
  ;;(global-unset-key (kbd "M-e"))
  ;;(global-unset-key (kbd "M-r"))
  ;;(global-unset-key (kbd "M-g"))
  ;;(global-unset-key (kbd "M-c"))
  ;;(global-unset-key (kbd "M-v"))
  ;;(global-unset-key (kbd "M-x"))
  (global-unset-key (kbd "M-m"))
  (global-unset-key (kbd "M-X"))
  (global-unset-key (kbd "M-b"))
  (global-unset-key (kbd "M-w"))
  ;;(global-unset-key (kbd "<prior>"))
  ;;(global-unset-key (kbd "<next>"))
  ;;(global-unset-key (kbd "<delete>"))
  ;;(global-unset-key (kbd "<backspace>"))
  ;;(global-unset-key (kbd "<C-delete>"))
  ;;(global-unset-key (kbd "<C-backspace>"))
  ;;(global-unset-key (kbd "<C-up>"))
  ;;(global-unset-key (kbd "<C-down>"))
  )
(add-hook 'cfg-hook:hotkey 'cfg:hotkeys)
