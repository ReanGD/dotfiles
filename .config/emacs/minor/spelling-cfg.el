(provide 'minor/spelling-cfg)

(defun lcl:spelling-add-to-dictionary (marked-text)
  (let* ((word (downcase (aref marked-text 0)))
         (dict (if (string-match "[a-zA-Z]" word)
                   (message "en_US.dic")
                 (message "ru_RU.dic")))
         (file (concat "~/.config/enchant/" dict)))
    (when (and file (file-writable-p file))
      (with-temp-buffer
        (insert word) (newline)
        (append-to-file (point-min) (point-max) file)
        (message "Added word \"%s\" to the \"%s\" dictionary" word dict))
      (wcheck-mode 0)
      (wcheck-mode 1))))

(defun lcl:spelling-action-menu (marked-text)
  (cons (cons "[Add to dictionary]" 'lcl:spelling-add-to-dictionary)
        (wcheck-parser-ispell-suggestions)))

(defun cfg:spelling ()
  (require 'wcheck-mode)
  (defun wcheck--choose-action-minibuffer (actions)
    (cdr
     (assoc
      (ido-completing-read "Choose " (mapcar #'car actions))
      actions)))
  (setq-default
   wcheck-language "All"
   wcheck-language-data
   '(("All"
      (program . "~/.config/emacs/bin/spell_check_text.sh")
      (action-program . "~/.config/emacs/bin/spell_check_word.sh")
      (action-parser . lcl:spelling-action-menu)
      (read-or-skip-faces
       ((emacs-lisp-mode c-mode c++-mode python-mode)
        read font-lock-comment-face)
       (nil))
      ))))
(add-hook 'cfg-hook:minor-mode 'cfg:spelling)

(cfg:add-package 'wcheck-mode)
