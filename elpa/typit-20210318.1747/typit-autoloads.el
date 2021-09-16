;;; typit-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "typit" "typit.el" (0 0 0 0))
;;; Generated autoloads from typit.el

(autoload 'typit-test "typit" "\
Run a typing test using the NUM most common words from the dictionary.

Dictionary is an array of words in `typit-dict'.  By default it's
English words ordered from most common to least common.

\(fn NUM)" t nil)

(autoload 'typit-basic-test "typit" "\
Basic typing test (top 200 words).

See `typit-test' for more information." t nil)

(autoload 'typit-advanced-test "typit" "\
Advanced typing test (top 1000 words).

See `typit-test' for more information." t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "typit" '("typit-")))

;;;***

;;;### (autoloads nil nil ("typit-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; typit-autoloads.el ends here
