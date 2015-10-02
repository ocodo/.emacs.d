;;; jist-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "jist" "jist.el" (22030 34881 221140 0))
;;; Generated autoloads from jist.el

(autoload 'jist-region "jist" "\
Create a private gist from BEG and END region.

When PUBLIC is not nil creates a public gist.

\(fn &key (BEG (and (use-region-p) (region-beginning))) (END (and (use-region-p) (region-end))) (PUBLIC nil) (AUTHORIZED nil))" t nil)

(autoload 'jist-auth-region "jist" "\
Create an authorized gist from an active region.

\(fn)" t nil)

(autoload 'jist-region-public "jist" "\
Create a public gist from an active region.

\(fn)" t nil)

(autoload 'jist-auth-region-public "jist" "\
Create a public and authorized gist from an active region.

\(fn)" t nil)

(autoload 'jist-buffer "jist" "\
Create a gist from the contents of the current buffer.

\(fn)" t nil)

(autoload 'jist-auth-buffer "jist" "\
Create an authorized gist from the contents of the current buffer.

\(fn)" t nil)

(autoload 'jist-buffer-public "jist" "\
Create a public gist from the contents of the current buffer.

\(fn)" t nil)

(autoload 'jist-auth-buffer-public "jist" "\
Create an authorized and public gist from the contents of the current buffer.

\(fn)" t nil)

(autoload 'jist-delete-gist "jist" "\
Delete gist with ID.

\(fn ID)" t nil)

(autoload 'jist-browse-gist "jist" "\
Show a gist with ID in a browser.

\(fn ID)" t nil)

(autoload 'jist-star-gist "jist" "\
Star a gist ID.

\(fn ID)" t nil)

(autoload 'jist-unstar-gist "jist" "\
Unstar a gist ID.

\(fn ID)" t nil)

(autoload 'jist-clone-gist "jist" "\
Close gist ID.

\(fn ID)" t nil)

(autoload 'jist-refetch-gists "jist" "\
Refetch the gists of a jist-list-mode buffer.

\(fn)" t nil)

(autoload 'jist-list "jist" "\
Show the list of gists.

\(fn &key (USER nil) (PUBLIC nil) (STARRED nil))" t nil)

(autoload 'jist-list-user "jist" "\
Show a list of gist of a github USER.

\(fn USER)" t nil)

(autoload 'jist-list-public "jist" "\
Show a list of public gists.

\(fn)" t nil)

(autoload 'jist-list-starred "jist" "\
Show a list of starred gists of the configured user.

\(fn)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; jist-autoloads.el ends here
