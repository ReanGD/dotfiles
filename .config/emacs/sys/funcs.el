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
