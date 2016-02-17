(provide 'minor/multi-compile-cfg)

(defun cfg:multi-compile ()
  (require 'multi-compile)
  (add-to-list
   'multi-compile-alist
   '(go-mode . (("go-build" "go build -v"
                 (multi-compile-locate-file-dir ".git"))
                ("go-build-and-run" "go build -v && echo 'build finish' && eval ./${PWD##*/}"
                 (multi-compile-locate-file-dir ".git"))))
   )

  )
(add-hook 'cfg-hook:minor-mode 'cfg:multi-compile)

(cfg:add-package 'multi-compile)
