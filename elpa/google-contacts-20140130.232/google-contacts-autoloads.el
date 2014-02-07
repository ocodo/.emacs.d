;;; google-contacts-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (google-contacts-async-api google-contacts-async
;;;;;;  google-contacts) "google-contacts" "google-contacts.el" (21236
;;;;;;  38958 0 0))
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

;;;### (autoloads (google-oauth-auth-and-store google-oauth-auth)
;;;;;;  "google-oauth" "google-oauth.el" (21236 38958 0 0))
;;; Generated autoloads from google-oauth.el

(autoload 'google-oauth-auth "google-oauth" "\
Request access to a resource.

\(fn RESOURCE-URL CLIENT-ID CLIENT-SECRET)" nil nil)

(autoload 'google-oauth-auth-and-store "google-oauth" "\
Request access to a Google resource and store it using `auth-source'.

\(fn RESOURCE-URL CLIENT-ID CLIENT-SECRET)" nil nil)

;;;***

;;;### (autoloads (google-contacts-to-org-contacts) "google-org-contacts"
;;;;;;  "google-org-contacts.el" (21236 38958 0 0))
;;; Generated autoloads from google-org-contacts.el

(autoload 'google-contacts-to-org-contacts "google-org-contacts" "\
Insert contacts in org-contacts format into BUFFER.
If QUERY-STRING is nil insert all contacts.

\(fn &optional BUFFER QUERY-STRING)" t nil)

;;;***

;;;### (autoloads nil nil ("google-contacts-gnus.el" "google-contacts-message.el"
;;;;;;  "google-contacts-pkg.el") (21236 38958 726610 0))

;;;***

(provide 'google-contacts-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; google-contacts-autoloads.el ends here
