;;; google-contacts-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "google-contacts" "google-contacts.el" (22653
;;;;;;  62755 762838 131000))
;;; Generated autoloads from google-contacts.el

(autoload 'google-contacts "google-contacts" "\


\(fn &optional QUERY-STRING FORCE-REFRESH)" t nil)

(autoload 'google-contacts-async "google-contacts" "\
Search Google Contacts for QUERY-STRING.

\(fn &optional QUERY-STRING FORCE-REFRESH)" t nil)

(autoload 'google-contacts-async-api "google-contacts" "\
Search Google Contacts for QUERY-STRING and call CALLBACK with the result as a list.
CBARGS are passed to CALLBACK.

\(fn QUERY-STRING CALLBACK &rest CBARGS)" nil nil)

;;;***

;;;### (autoloads nil "google-oauth" "google-oauth.el" (22653 62755
;;;;;;  770838 303000))
;;; Generated autoloads from google-oauth.el

(autoload 'google-oauth-auth "google-oauth" "\
Request access to a resource.

\(fn RESOURCE-URL CLIENT-ID CLIENT-SECRET)" nil nil)

(autoload 'google-oauth-auth-and-store "google-oauth" "\
Request access to a Google resource and store it using `auth-source'.

\(fn RESOURCE-URL CLIENT-ID CLIENT-SECRET)" nil nil)

;;;***

;;;### (autoloads nil "google-org-contacts" "google-org-contacts.el"
;;;;;;  (22653 62755 778838 475000))
;;; Generated autoloads from google-org-contacts.el

(autoload 'google-contacts-to-org-contacts "google-org-contacts" "\
Insert contacts in org-contacts format into BUFFER.
If QUERY-STRING is nil insert all contacts.

\(fn &optional BUFFER QUERY-STRING)" t nil)

;;;***

;;;### (autoloads nil nil ("google-contacts-gnus.el" "google-contacts-message.el"
;;;;;;  "google-contacts-pkg.el") (22653 62755 806839 75000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; google-contacts-autoloads.el ends here
