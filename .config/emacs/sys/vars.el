(provide 'sys/vars)


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
