;;; elfeed-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "elfeed" "elfeed.el" (23344 45060 232846 964000))
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

;;;***

;;;### (autoloads nil "elfeed-link" "elfeed-link.el" (23344 45060
;;;;;;  236847 58000))
;;; Generated autoloads from elfeed-link.el

(autoload 'elfeed-link-store-link "elfeed-link" "\
Store a link to an elfeed search or entry buffer.

When storing a link to an entry, automatically extract all the
entry metadata.  These can be used in the capture templates as
%:elfeed-entry-<prop>.  See `elfeed-entry--create' for the list
of available props.

\(fn)" nil nil)

(autoload 'elfeed-link-open "elfeed-link" "\
Jump to an elfeed entry or search.

Depending on what FILTER-OR-ID looks like, we jump to either
search buffer or show a concrete entry.

\(fn FILTER-OR-ID)" nil nil)

(eval-after-load 'org `(funcall ',(lambda nil (if (version< (org-version) "9.0") (with-no-warnings (org-add-link-type "elfeed" #'elfeed-link-open) (add-hook 'org-store-link-functions #'elfeed-link-store-link)) (with-no-warnings (org-link-set-parameters "elfeed" :follow #'elfeed-link-open :store #'elfeed-link-store-link))))))

;;;***

;;;### (autoloads nil "elfeed-search" "elfeed-search.el" (23344 45060
;;;;;;  244847 247000))
;;; Generated autoloads from elfeed-search.el

(autoload 'elfeed-search-bookmark-handler "elfeed-search" "\
Jump to an elfeed-search bookmarked location.

\(fn RECORD)" nil nil)

(autoload 'elfeed-search-desktop-restore "elfeed-search" "\
Restore the state of an elfeed-search buffer on desktop restore.

\(fn FILE-NAME BUFFER-NAME SEARCH-FILTER)" nil nil)

(add-to-list 'desktop-buffer-mode-handlers '(elfeed-search-mode . elfeed-search-desktop-restore))

;;;***

;;;### (autoloads nil nil ("elfeed-csv.el" "elfeed-curl.el" "elfeed-db.el"
;;;;;;  "elfeed-lib.el" "elfeed-log.el" "elfeed-pkg.el" "elfeed-show.el"
;;;;;;  "xml-query.el") (23344 45060 244847 247000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; elfeed-autoloads.el ends here
