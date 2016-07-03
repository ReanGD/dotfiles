;;; volatile-highlights-cfg.el --- Configure volatile-highlights
;;; Commentary:
;;; Code:
(require 'sys/packages)
(require 'volatile-highlights nil t)

(defun cfg:volatile-highlights ()
  "Configure volatile-highlights."
  (volatile-highlights-mode t))
(add-hook 'cfg-hook:minor-mode 'cfg:volatile-highlights)

(cfg:add-package 'volatile-highlights)

(provide 'minor/volatile-highlights-cfg)
;;; volatile-highlights-cfg.el ends here
