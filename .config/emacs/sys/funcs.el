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

(defun cfg:format-buffer ()
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)))

(defun cfg:scroll-screen (arg)
  (interactive)
  (let ((pos (point))
        (col (current-column)))
    (scroll-up arg)
    (if (pos-visible-in-window-p pos)
        (goto-char pos)
      (if (or (eq last-command 'next-line)
              (eq last-command 'previous-line))
          (move-to-column temporary-goal-column)
        (move-to-column col)
        (setq temporary-goal-column col))
      (setq this-command 'next-line))))

(defun cfg:scroll-screen-up ()
  (interactive)
  (cfg:scroll-screen -1))

(defun cfg:scroll-screen-down ()
  (interactive)
  (cfg:scroll-screen 1))

(defun cfg:page-up ()
  (interactive)
  (condition-case nil (scroll-down)
    (beginning-of-buffer (goto-char (point-min)))))

(defun cfg:page-down ()
  (interactive)
  (condition-case nil (scroll-up)
    (end-of-buffer (goto-char (point-max)))))

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

(defun cfg:backward-delete-tab-whitespace ()
  (interactive)
  (let ((p (point)))
    (if (and (eq indent-tabs-mode nil)
             (>= p tab-width)
             (eq (% (current-column) tab-width) 0)
             (string-match "^\\s-+$" (buffer-substring-no-properties (- p tab-width) p)))
        (delete-backward-char tab-width)
      (delete-backward-char 1))))

(defun cfg:what-face (pos)
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))
