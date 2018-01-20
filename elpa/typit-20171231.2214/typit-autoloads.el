;;; typit-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "typit" "typit.el" (23138 47674 284050 226000))
;;; Generated autoloads from typit.el

(autoload 'typit-test "typit" "\
Run typing test with using NUM most common words from dictionary.

Dictionary is an array of words in `typit-dict'.  By default it's
English words ordered from most common to least common.  You can
let-bind the variable and change it, it's recommended to use at
least 1000 words so `typit-advanced-test' could work properly.

\(fn NUM)" t nil)

(autoload 'typit-basic-test "typit" "\
Basic typing test (top 200 words).

See `typit-test' for more information.

\(fn)" t nil)

(autoload 'typit-advanced-test "typit" "\
Advanced typing test (top 1000 words).

See `typit-test' for more information.

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("typit-pkg.el") (23138 47674 280050 209000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; typit-autoloads.el ends here
