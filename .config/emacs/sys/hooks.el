(provide 'sys/hooks)


(defcustom cfg-hook:ui nil
  "ui load"
  :type 'hook
  :group 'local-config)

(defcustom cfg-hook:minor-mode nil
  "minor-mode load"
  :type 'hook
  :group 'local-config)

(defcustom cfg-hook:major-mode nil
  "major-mode load"
  :type 'hook
  :group 'local-config)

(defcustom cfg-hook:session nil
  "after init session load"
  :type 'hook
  :group 'local-config)

(defcustom cfg-hook:hotkey nil
  "set hotkeys"
  :type 'hook
  :group 'local-config)

