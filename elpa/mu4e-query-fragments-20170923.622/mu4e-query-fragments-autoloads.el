;;; mu4e-query-fragments-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "mu4e-query-fragments" "mu4e-query-fragments.el"
;;;;;;  (23360 15922 611368 300000))
;;; Generated autoloads from mu4e-query-fragments.el

(defvar mu4e-query-fragments-list nil "\
Define query fragments available in `mu4e' searches and bookmarks.
List of (FRAGMENT . EXPANSION), where FRAGMENT is the string to be
substituted and EXPANSION is the query string to be expanded.

FRAGMENT should be an unique text symbol that doesn't conflict with the
regular mu4e/xapian search syntax or previous fragments. EXPANSION is
expanded as (EXPANSION), composing properly with boolean operators and
can contain fragments in itself.

Example:

\(setq mu4e-query-fragments-list
   '((\"%junk\" . \"maildir:/Junk OR subject:SPAM\")
     (\"%hidden\" . \"flag:trashed OR %junk\")))")

(custom-autoload 'mu4e-query-fragments-list "mu4e-query-fragments" t)

(defvar mu4e-query-fragments-append nil "\
Query fragment appended to new searches by `mu4e-query-fragments-search'.")

(custom-autoload 'mu4e-query-fragments-append "mu4e-query-fragments" t)

(autoload 'mu4e-query-fragments-expand "mu4e-query-fragments" "\
Expand fragments defined in `mu4e-query-fragments-list' in QUERY.

\(fn QUERY)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; mu4e-query-fragments-autoloads.el ends here
