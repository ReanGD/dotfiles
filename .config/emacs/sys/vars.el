(provide 'sys/vars)


(defcustom cfg-var:work-dir nil
  "set new user-emacs-directory or nill for default"
  :type 'string
  :group 'local-config)

(defcustom cfg-var:packages '()
  "list packages for install"
  :type '(list (cons symbol function))
  :group 'local-config)

(defcustom cfg-var:theme "light"
  "Theme name (now light/dark)"
  :type 'string
  :group 'local-config)

(defcustom cfg-var:find-file 'find-file
  "function call for file find"
  :type 'function
  :group 'local-config)

(defcustom cfg-var:find-buffer 'switch-to-buffer
  "function call for buffer find"
  :type 'function
  :group 'local-config)

(defcustom cfg-var:find-command 'execute-extended-command
  "function call for command find"
  :type 'function
  :group 'local-config)

(defcustom cfg-var:find-major-command 'execute-extended-command
  "function call for major mode command find"
  :type 'function
  :group 'local-config)
