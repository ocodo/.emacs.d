;;; codesearch-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "codesearch" "codesearch.el" (0 0 0 0))
;;; Generated autoloads from codesearch.el

(autoload 'codesearch-build-index "codesearch" "\
Add the contents of `dir' to `index-file'.

\(fn DIR INDEX-FILE)" t nil)

(autoload 'codesearch-update-index "codesearch" "\
Rescan all of the directories currently in the index, updating
the index with the new contents.

\(fn)" t nil)

(autoload 'codesearch-reset "codesearch" "\
Reset (delete) the codesearch index.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "codesearch" '("codesearch-")))

;;;***

;;;### (autoloads nil "listing-codesearch" "listing-codesearch.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from listing-codesearch.el

(autoload 'listing-codesearch-search "listing-codesearch" "\
Search files matching `file-pattern'in the index for `pattern'.

\(fn PATTERN FILE-PATTERN)" t nil)

(autoload 'listing-codesearch-list-directories "listing-codesearch" "\
List the directories currently being indexed.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "listing-codesearch" '("listing-codesearch-")))

;;;***

;;;### (autoloads nil nil ("codesearch-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; codesearch-autoloads.el ends here
