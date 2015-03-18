(provide 'minor/test-cfg)


(defun cfg:test ()
	(setq helm-dash-docsets-path "/home/rean/.local/share/Zeal/Zeal")
  (window-numbering-mode t)
  )
(add-hook 'cfg-hook:minor-mode 'cfg:test)

(cfg:add-package 'zeal-at-point)
(cfg:add-package 'helm-dash)
(cfg:add-package 'window-numbering)
(cfg:add-package 'w3m)

;; (defvar zeal-doc-zeal-path
;;   "~/.local/share/Zeal/Zeal")

;; (defun zeal-doc--path-join (dir-list)
;;   (reduce
;;    (lambda (path1 path2)
;;      (concat (file-name-as-directory path1) path2))
;;    dir-list))

;; (defun zeal-doc--get-db-path (db-name)
;;   (expand-file-name
;;    (zeal-doc--path-join
;;     (list zeal-doc-zeal-path "docsets" (concat db-name ".docset") "Contents" "Resources" "docSet.dsidx"))))

;; (defun zeal-doc--db-call (db-path sql)
;;   (split-string
;;    (with-output-to-string
;;      (call-process-shell-command
;;       (format "sqlite3 \"%s\" \"%s\"" db-path sql) nil standard-output)) "\n" t))

;; (defun zeal-doc--is-simple-db-type (db-path)
;;   (let ((type_sql "SELECT name FROM sqlite_master WHERE type = 'table' LIMIT 1"))
;;     (equal "searchIndex" (car (zeal-doc--db-call db-path type_sql)))))

;; (defun zeal-doc--generate-like (field pattern)
;;   (format "%s like '%%%s%%'" field pattern))

;; (defun zeal-doc--generate-sql-long (pattern)
;;   (format "SELECT ty.ZTYPENAME, t.ZTOKENNAME, f.ZPATH FROM ZTOKEN t, ZTOKENTYPE ty, ZFILEPATH f, ZTOKENMETAINFORMATION m WHERE ty.Z_PK = t.ZTOKENTYPE AND f.Z_PK = m.ZFILE AND m.ZTOKEN = t.Z_PK AND %s ORDER BY LOWER(t.ZTOKENNAME) LIMIT 100;" (zeal-doc--generate-like "t.ZTOKENNAME" pattern)))

;; (defun zeal-doc--generate-sql-simple (pattern)
;;   (format "SELECT t.type, t.name, t.path FROM searchIndex t WHERE %s ORDER BY LOWER(t.name) LIMIT 100;" (zeal-doc--generate-like "t.name" pattern)))

;; (defun zeal-doc--generate-sql (pattern is-simple)
;;   (if is-simple
;;       (zeal-doc--generate-sql-simple pattern)
;;     (zeal-doc--generate-sql-long pattern)))

;; (defun zeal-doc--db-query (db-name pattern)
;;   (let ((db-path (zeal-doc--get-db-path db-name)))
;;     (zeal-doc--db-call db-path
;;                        (zeal-doc--generate-sql pattern
;;                                                (zeal-doc--is-simple-db-type db-path)))))

;; (defun zeal-doc--query (db-names pattern)
;;   (mapcar (lambda (x) (split-string x "|" t))
;;           (reduce 'append
;;                   (mapcar (lambda(db-name) (zeal-doc--db-query db-name pattern))
;;                           (split-string db-names)))))


;; (progn
;;   (setq tbl (make-hash-table :test 'equal))
;;   (setq ido-list nil)
;;   (setq ind 0)
;;   (dolist (it (zeal-doc--query "C++ Boost" "duration"))
;;     (setq key (format "%i.%s" ind (cadr it)))
;;     (setq val (car (last it)))
;;     (puthash key val tbl)
;;     (add-to-list 'ido-list key t)
;;     (setq ind (1+ ind))
;;     )
;;   (setq base "/home/rean/.local/share/Zeal/Zeal/docsets/C++.docset/Contents/Resources/Documents/")
;;   (eww-open-file
;;    (concat base
;;            (gethash
;;             (ido-completing-read "Help:" ido-list)
;;             tbl)))
;;   )

;; (car (last '(1 2 3)))

;; (setq mylist (list "red" "blue" "yellow" "clear" "i-dont-know"))
;; (setq mylist (list '("a" "aa") '("b" "bb")))
;; (ido-completing-read "What, ... is your favorite color? " mylist "qwe")


;; (ido-completing-read "Channel:" 
;;                      (save-excursion
;;                        (delq
;;                         nil
;;                         (mapcar (lambda (buf)
;;                                   (when (buffer-live-p buf)
;;                                     (with-current-buffer buf
;;                                       (and (eq major-mode 'erc-mode)
;;                                            (buffer-name buf)))))
;;                                 (buffer-list)))))
