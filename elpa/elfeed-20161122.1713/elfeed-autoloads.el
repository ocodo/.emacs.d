;;; elfeed-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "elfeed" "elfeed.el" (0 0 0 0))
;;; Generated autoloads from elfeed.el

(autoload 'elfeed-update "elfeed" "\
Update all the feeds in `elfeed-feeds'.

\(fn)" t nil)

(autoload 'elfeed "elfeed" "\
Enter elfeed.

\(fn)" t nil)

(autoload 'elfeed-load-opml "elfeed" "\
Load feeds from an OPML file into `elfeed-feeds'.
When called interactively, the changes to `elfeed-feeds' are
saved to your customization file.

\(fn FILE)" t nil)

(autoload 'elfeed-export-opml "elfeed" "\
Export the current feed listing to OPML-formatted FILE.

\(fn FILE)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "elfeed" '("elfeed-")))

;;;***

;;;### (autoloads nil "elfeed-csv" "elfeed-csv.el" (0 0 0 0))
;;; Generated autoloads from elfeed-csv.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "elfeed-csv" '("elfeed-csv-")))

;;;***

;;;### (autoloads nil "elfeed-curl" "elfeed-curl.el" (0 0 0 0))
;;; Generated autoloads from elfeed-curl.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "elfeed-curl" '("elfeed-curl-")))

;;;***

;;;### (autoloads nil "elfeed-db" "elfeed-db.el" (0 0 0 0))
;;; Generated autoloads from elfeed-db.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "elfeed-db" '("elfeed-" "with-elfeed-db-visit")))

;;;***

;;;### (autoloads nil "elfeed-lib" "elfeed-lib.el" (0 0 0 0))
;;; Generated autoloads from elfeed-lib.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "elfeed-lib" '("elfeed-")))

;;;***

;;;### (autoloads nil "elfeed-log" "elfeed-log.el" (0 0 0 0))
;;; Generated autoloads from elfeed-log.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "elfeed-log" '("elfeed-log")))

;;;***

;;;### (autoloads nil "elfeed-search" "elfeed-search.el" (0 0 0 0))
;;; Generated autoloads from elfeed-search.el

(autoload 'elfeed-search-bookmark-handler "elfeed-search" "\
Jump to an elfeed-search bookmarked location.

\(fn RECORD)" nil nil)

(autoload 'elfeed-search-desktop-restore "elfeed-search" "\
Restore the state of an elfeed-search buffer on desktop restore.

\(fn FILE-NAME BUFFER-NAME SEARCH-FILTER)" nil nil)

(add-to-list 'desktop-buffer-mode-handlers '(elfeed-search-mode . elfeed-search-desktop-restore))

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "elfeed-search" '("elfeed-s")))

;;;***

;;;### (autoloads nil "elfeed-show" "elfeed-show.el" (0 0 0 0))
;;; Generated autoloads from elfeed-show.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "elfeed-show" '("elfeed-")))

;;;***

;;;### (autoloads nil "xml-query" "xml-query.el" (0 0 0 0))
;;; Generated autoloads from xml-query.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "xml-query" '("xml-query")))

;;;***

;;;### (autoloads nil nil ("elfeed-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; elfeed-autoloads.el ends here
