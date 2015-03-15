(provide 'sys/funcs)


(defun cfg-func:find-file ()
  (interactive)
  (call-interactively cfg-var:find-file)
  (if (not (equal cfg-var:find-file 'find-file))
      (run-hooks 'find-file-hook)))

(defun cfg-func:find-buffer ()
  (interactive)
  (call-interactively cfg-var:find-buffer))

(defun cfg-func:find-command ()
  (interactive)
  (call-interactively cfg-var:find-command))

(defun cfg-func:find-major-command ()
  (interactive)
  (call-interactively cfg-var:find-major-command))

(defun cfg-func:compile ()
  (interactive)
  (save-buffer)
  (setq-local compilation-read-command nil)
  (call-interactively 'compile))

(defun cfg:format-buffer ()
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)))

(defun cfg:scroll-up ()
  (interactive)
  (condition-case nil (scroll-up)
    (end-of-buffer (goto-char (point-max)))))

(defun cfg:scroll-down ()
  (interactive)
  (condition-case nil (scroll-down)
    (beginning-of-buffer (goto-char (point-min)))))

(defun cfg:delete-word (arg)
  (interactive "p")
  (delete-region (point) (progn (forward-word arg) (point))))

(defun cfg:backward-delete-word (arg)
  (interactive "p")
  (cfg:delete-word (- arg)))

(defun cfg:delete-line ()
  (interactive)
  (delete-region
   (point)
   (save-excursion (move-end-of-line 1) (point)))
  (delete-char 1))

(defun cfg:backspace-soft-tab-once ()
  (interactive)
  "Backspace one \"soft\" tab, or a tab made of spaces."
  ;; If we're at the beginning of the buffer, just delete a char
  (if (<= (point) tab-width)
      (delete-backward-char 1)
    ;; Get the last tab-width characters before the cursor's location
    (let ((prev-string (buffer-substring (- (point) tab-width)
                                         (point))))
      ;; If the current column is a multiple of tab-width, and the
      ;; last tab-width chars were all spaces, delete all of
      ;; them. Otherwise, just delete one character.
      (if (and (zerop (% (current-column) tab-width))
               (null (remove-if (lambda (x)
                                  (eql ?\s x))
                                (coerce prev-string
                                        'list))))
          (delete-backward-char tab-width)
        (delete-backward-char 1)))))
