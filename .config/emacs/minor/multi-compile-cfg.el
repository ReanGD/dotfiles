(provide 'minor/multi-compile-cfg)

(defun cfg:multi-compile ()
  (require 'multi-compile)
  ;; (add-to-list
  ;;  'multi-compile-alist
  ;;  '(go-mode . (("go-run" "go build -v && echo 'build finish' && eval ./${PWD##*/}"
  ;;                (locate-dominating-file buffer-file-name ".git")
  ;;                ;; (multi-compile-locate-file-dir ".git")
  ;;                )
  ;;               )))
  (defun string/starts-with (string prefix)
    "Return t if STRING starts with prefix."
    (and (stringp string) (string-match (rx-to-string `(: bos ,prefix) t) string)))

  (setq multi-compile-alist
        '(
          (rust-mode . (("rust-debug" . "cargo run")
                        ("rust-release" . "cargo run --release")
                        ("rust-test" . "cargo test")))
          ("\\.txt\\'" . (("print-filename" . "echo %file-name")))
          ("*scratch*" . (("print-hello" . "echo 'hello'")))
          ("\\.*" . (("item-for-all" . "echo 'I am item for all'")))
          ((string/starts-with buffer-file-name "/home/") . (("item-for-home" . "echo %file-name")))
          (go-mode . (("go-build" "go build -v"
                       (locate-dominating-file buffer-file-name ".git"))
                      ("go-build-and-run" "go build -v && echo 'build finish' && eval ./${PWD##*/}"
                       (multi-compile-locate-file-dir ".git"))))
          ))

  )
(add-hook 'cfg-hook:minor-mode 'cfg:multi-compile)

(cfg:add-package 'multi-compile)

;; (setq multi-compile-alist
;;   '(
;;     ((t) . (("any" . "make --no-print-directory -C %make-dir")))
;;     (rust-mode . (("rust-debug" . "cargo run")
;;                   ("rust-release" . "cargo run --release")
;;                   ("rust-test" . "cargo test")))
;;     (c++-mode . (("cpp-run" . "make --no-print-directory -C %make-dir")))
;;     (go-mode . (("go-run" "go build -v && echo 'build finish' && eval ./${PWD##*/}"
;;                              ;; (locate-dominating-file buffer-file-name ".git")
;;                              (multi-compile-locate-file-dir ".git")
;;                              )))
;;     ))

;; (listp (car (car multi-compile-alist)))
;; (symbolp (car (car (cdr multi-compile-alist))))

;; (defcustom multi-compile-alist
;;   '(
;;     (rust-mode . (("rust-debug" . "cargo run")
;;                   ("rust-release" . "cargo run --release")
;;                   ("rust-test" . "cargo test")))
;;     (c++-mode . (("cpp-run" . "make --no-print-directory -C %make-dir")))
;;     )
;;   "Alist of filename patterns vs corresponding format control strings."
;;   :type '(repeat
;;           (cons
;;            (choice :tag "Key"
;;                    (regexp :tag "Filename or buffer pattern")
;;                    (function :tag "Major-mode")
;;                    (sexp :tag "Expression")
;;                    )
;;            (repeat :tag "Settings"
;;                    (choice :tag "Type"
;;                            (cons :tag "Default compilation directory"
;;                                  (string :tag "Menu item")
;;                                  (string :tag "Command"))
;;                            (list :tag "Set compilation directory"
;;                                  (string :tag "Menu item")
;;                                  (string :tag "Command")
;;                                  (sexp :tag "Expression returns a compilation root"))
;;                            ))))
;;   :group 'multi-compile)
