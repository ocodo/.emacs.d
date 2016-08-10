;;; yaoddmuse-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "yaoddmuse" "yaoddmuse.el" (0 0 0 0))
;;; Generated autoloads from yaoddmuse.el

(autoload 'yaoddmuse-edit "yaoddmuse" "\
Edit a page on a wiki.
WIKINAME is the name of the wiki as defined in `yaoddmuse-wikis',
PAGENAME is the pagename of the page you want to edit.
Use a PREFIX argument to force a reload of the page.

\(fn &optional WIKINAME PAGENAME PREFIX)" t nil)

(autoload 'yaoddmuse-edit-default "yaoddmuse" "\
Edit a page with default wiki `yaoddmuse-default-wiki'.
Use a PREFIX argument to force a reload of the page.

\(fn PREFIX)" t nil)

(autoload 'yaoddmuse-post-buffer "yaoddmuse" "\
Post the BUFFER to the current wiki.
The current wiki is taken from `yaoddmuse-wiki'.
The POST-BUFFER is the buffer you want post,
Default will read buffer name if POST-BUFFER is void.
SUMMARY is summary name for post.
If PREFIX is non-nil, will view page after post successful.

\(fn &optional POST-BUFFER SUMMARY PREFIX)" t nil)

(autoload 'yaoddmuse-post-current-buffer "yaoddmuse" "\
Post current buffer to current wiki.
The current wiki is taken from `yaoddmuse-wiki'.
Use a PREFIX argument to browse page after post successful.

\(fn PREFIX)" t nil)

(autoload 'yaoddmuse-post-file "yaoddmuse" "\
Post file to current wiki.
The current wiki is taken from `yaoddmuse-wiki'.
FILENAME is file name you want post.
WIKINAME is wiki name for post.
PAGENAME is page name for post.
SUMMARY is summary for post.
If PREFIX is non-nil, will view page after post successful.

\(fn &optional FILENAME WIKINAME PAGENAME SUMMARY PREFIX)" t nil)

(autoload 'yaoddmuse-post-file-default "yaoddmuse" "\
Post file to default wiki.
If PREFIX is non-nil, will view page after post successful.

\(fn PREFIX)" t nil)

(autoload 'yaoddmuse-post-library "yaoddmuse" "\
Post library to current wiki.
The current wiki is taken from `yaoddmuse-wikis'.
LIBRARY is library name you want post.
WIKINAME is wiki name for post.
PAGENAME is page name for post.
SUMMARY is summary for post.
If PREFIX is non-nil, will view page after post successful.

\(fn &optional LIBRARY WIKINAME PAGENAME SUMMARY PREFIX)" t nil)

(autoload 'yaoddmuse-post-library-default "yaoddmuse" "\
Post library to default wiki.
Use a PREFIX argument to browse page after post successful.

\(fn PREFIX)" t nil)

(autoload 'yaoddmuse-post-dired "yaoddmuse" "\
Post dired marked files to current wiki.
The current wiki is taken from `yaoddmuse-wikis'.
WIKINAME is wiki name for post.
SUMMARY is summary for post.
If PREFIX is non-nil, will view page after post successful.

\(fn &optional WIKINAME SUMMARY PREFIX)" t nil)

(autoload 'yaoddmuse-post-dired-default "yaoddmuse" "\
Post dired marked files to default wiki.
Use a PREFIX argument to browse page after post successful.

\(fn PREFIX)" t nil)

(autoload 'yaoddmuse-post-screenshot "yaoddmuse" "\
Post screenshot to current wiki.
The current wiki is taken from `yaoddmuse-wikis'.
WIKINAME is wiki name for post.
SUMMARY is summary for post.
If PREFIX is non-nil, will view page after post successful.

\(fn &optional WIKINAME SUMMARY PREFIX)" t nil)

(autoload 'yaoddmuse-post-screenshot-default "yaoddmuse" "\
Post screenshot to default wiki.
Use a PREFIX argument to browse page after post successful.

\(fn PREFIX)" t nil)

(autoload 'yaoddmuse-browse-page "yaoddmuse" "\
Browse special page in wiki.
WIKINAME is the name of the wiki as defined in `yaoddmuse-wikis',
PAGENAME is the pagename of the page you want to edit.

\(fn &optional WIKINAME PAGENAME)" t nil)

(autoload 'yaoddmuse-browse-page-default "yaoddmuse" "\
Brose special page with `yaoddmuse-default-wiki'.

\(fn)" t nil)

(autoload 'yaoddmuse-browse-page-diff "yaoddmuse" "\
Browse special page diff in wiki.
WIKINAME is the name of the wiki as defined in `yaoddmuse-wikis',
PAGENAME is the pagename of the page you want to edit.

\(fn &optional WIKINAME PAGENAME)" t nil)

(autoload 'yaoddmuse-browse-page-default-diff "yaoddmuse" "\
Browse special page with `yaoddmuse-default-wiki'.

\(fn)" t nil)

(autoload 'emacswiki "yaoddmuse" "\
Edit a page on the EmacsWiki.
PAGENAME is the pagename of the page you want to edit.
Use a PREFIX argument to force a reload of the page.

\(fn &optional PAGENAME PREFIX)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "yaoddmuse" '("yaoddmuse-" "emacswiki-post")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; yaoddmuse-autoloads.el ends here
