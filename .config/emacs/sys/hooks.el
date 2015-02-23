(provide 'sys/hooks)

(defcustom cfg-hook:ui nil
  "ui load"
  :type 'hook
  :group 'my-config)

(defcustom cfg-hook:mode nil
  "common mode load (autocomplete and etc.)"
  :type 'hook
  :group 'my-config)

(defcustom cfg-hook:settings nil
  "settings load (c++ settings and etc.)"
  :type 'hook
  :group 'my-config)

(defcustom cfg-hook:session nil
  "session load"
  :type 'hook
  :group 'my-config)

(defcustom cfg-hook:hotkey nil
  "set hotkeys"
  :type 'hook
  :group 'my-config)

