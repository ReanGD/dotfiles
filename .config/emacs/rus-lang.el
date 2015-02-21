(provide 'rus-lang)


;; Горячие клавиши в других раскладках
(defun reverse-input-method (input-method)
  "Build the reverse mapping of single letters from INPUT-METHOD."
  (interactive
   (list (read-input-method-name "Use input method (default current): ")))
  (if (and input-method (symbolp input-method))
      (setq input-method (symbol-name input-method)))
  (let ((current current-input-method)
  (modifiers '(nil (control) (meta) (control meta))))
    (when input-method
      (activate-input-method input-method))
    (when (and current-input-method quail-keyboard-layout)
      (dolist (map (cdr (quail-map)))
  (let* ((to (car map))
         (from (quail-get-translation
          (cadr map) (char-to-string to) 1)))
    (when (and (characterp from) (characterp to))
      (dolist (mod modifiers)
        (define-key local-function-key-map
    (vector (append mod (list from)))
    (vector (append mod (list to)))))))))
    (when input-method
      (activate-input-method current))))

;; -------------------- hooks --------------------

(defun rus-lang-packages()
  '())

(defun rus-lang-pre-load ()
  )

(defun rus-lang-load()
  (reverse-input-method 'russian-computer))

(defun rus-lang-post-load ()
  (reverse-input-method 'russian-computer))
