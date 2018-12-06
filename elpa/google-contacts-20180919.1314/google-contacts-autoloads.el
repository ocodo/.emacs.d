;;; google-contacts-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "google-contacts" "google-contacts.el" (0 0
;;;;;;  0 0))
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

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "google-contacts" '("google-contacts-" "xml-node-")))

;;;***

;;;### (autoloads nil "google-contacts-gnus" "google-contacts-gnus.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from google-contacts-gnus.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "google-contacts-gnus" '("google-contacts-gnus-")))

;;;***

;;;### (autoloads nil "google-contacts-message" "google-contacts-message.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from google-contacts-message.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "google-contacts-message" '("google-contacts-")))

;;;***

;;;### (autoloads nil "google-oauth" "google-oauth.el" (0 0 0 0))
;;; Generated autoloads from google-oauth.el

(autoload 'google-oauth-auth "google-oauth" "\
Request access to a resource.

\(fn RESOURCE-URL CLIENT-ID CLIENT-SECRET)" nil nil)

(autoload 'google-oauth-auth-and-store "google-oauth" "\
Request access to a Google resource and store it using `auth-source'.

\(fn RESOURCE-URL CLIENT-ID CLIENT-SECRET)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "google-oauth" '("google-oauth-")))

;;;***

;;;### (autoloads nil "google-org-contacts" "google-org-contacts.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from google-org-contacts.el

(autoload 'google-contacts-to-org-contacts "google-org-contacts" "\
Insert contacts in org-contacts format into BUFFER.
If QUERY-STRING is nil insert all contacts.

\(fn &optional BUFFER QUERY-STRING)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "google-org-contacts" '("google-org-contacts--insert")))

;;;***

;;;### (autoloads nil nil ("google-contacts-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; google-contacts-autoloads.el ends here
