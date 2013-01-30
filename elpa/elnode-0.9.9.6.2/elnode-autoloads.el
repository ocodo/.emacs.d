;;; elnode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (elnode-do-init elnode-init define-elnode-handler
;;;;;;  elnode-webserver elnode-make-webserver elnode--webserver-handler-proc
;;;;;;  elnode-hostpath-default-table elnode-start elnode-list elnode-app)
;;;;;;  "elnode" "elnode.el" (20736 22420))
;;; Generated autoloads from elnode.el

(defconst elnode-config-directory (expand-file-name (concat user-emacs-directory "elnode/")) "\
The config directory for elnode to store peripheral files.

This is used as a base for other constant directory or file
names (the elnode auth database is a file in this directory, the
elnode webserver has a docroot directory in this directory).

It is based on the `user-emacs-directory' which always seems to
be set, even when emacs is started with -Q.")

(autoload 'elnode-app "elnode" "\
A macro that sets up the boring boilerplate for Elnode apps.

This sets up lexical binding, captures the module's parent
directory in DIR-VAR, require's `cl' and any other features you
list.  Use it like this:

 (elnode-app my-app-dir esxml mongo-elnode)

Once used you can access the variable `my-app-dir' as the dirname
of your module (which is useful for serving files and such).

\(fn DIR-VAR &rest FEATURES)" nil (quote macro))

(put 'elnode-app 'lisp-indent-function '2)

(autoload 'elnode-list "elnode" "\


\(fn)" t nil)

(autoload 'elnode-start "elnode" "\
Start a server using REQUEST-HANDLER.

REQUEST-HANDLER will handle requests on PORT on HOST (which is
'localhost' by default).

REQUEST-HANDLER is a function which is called with the request.
The function is called with one argument, which is the
http-connection.

You can use functions such as `elnode-http-start' and
`elnode-http-send-body' to send the http response.

Example:

  (defun nic-server (httpcon)
    (elnode-http-start httpcon 200 '((\"Content-Type: text/html\")))
    (elnode-http-return httpcon \"<html><b>BIG!</b></html>\")
x  )
  (elnode-start 'nic-server)

Now visit http://127.0.0.1:8000

If PORT is non-nil, then run server on PORT, otherwise default to
8000.

If HOST is non-nil, then run the server on the specified local IP
address, otherwise use localhost.  A few names are predefined:

  \"localhost\" is 127.0.0.1
  \"*\" is 0.0.0.0

Additionally, you may specifiy an IP address, e.g \"1.2.3.4\"

Note that although HOST may be specified, elnode does not
disambiguate on running servers by HOST.  So you cannot start two
elnode servers on the same port on different hosts.

\(fn REQUEST-HANDLER &key PORT (host \"localhost\") (defer-mode :managed))" t nil)

(defvar elnode-hostpath-default-table '(("[^/]+//wiki/\\(.*\\)" . elnode-wikiserver) ("[^/]+//\\(.*\\)" . elnode-webserver)) "\
Defines mappings for `elnode-hostpath-default-handler'.

This is the default mapping table for Elnode, out of the box. If
you customize this then elnode will serve these hostpath mappings
by just loading Elnode.

By default the table maps everything to
`elnode-webserver'. Unless you're happy with the default you
should probably get rid of the everything path because it will
interfere with any other mappings you add.")

(custom-autoload 'elnode-hostpath-default-table "elnode" t)

(defconst elnode-config-directory (expand-file-name (concat user-emacs-directory "elnode/")) "\
The config directory for elnode to store peripheral files.

This is used as a base for other constant directory or file
names (the elnode auth database is a file in this directory, the
elnode webserver has a docroot directory in this directory).

It is based on the `user-emacs-directory' which always seems to
be set, even when emacs is started with -Q.")

(autoload 'elnode--webserver-handler-proc "elnode" "\
Actual webserver implementation.

Do webserving to HTTPCON from the DOCROOT using the MIME-TYPES
for meta information.

This is not a real handler (because it takes more than the
HTTPCON) but it is called directly by the real webserver
handlers.

\(fn HTTPCON DOCROOT MIME-TYPES)" nil nil)

(autoload 'elnode-make-webserver "elnode" "\
Make a webserver interactively, for DOCROOT on PORT.

An easy way for a user to make a webserver for a particular
directory.

\(fn DOCROOT PORT)" t nil)

(autoload 'elnode-webserver "elnode" "\
A simple webserver that serves documents out of `elnode-webserver-docroot'.

This is just an example of an elnode webserver, but it may be all
that is needed most of the time.

See `elnode-webserver-handler-maker' for more possibilities for
making webserver functions.

HTTPCON is the HTTP connection to the user agent.

\(fn HTTPCON)" nil nil)

(autoload 'define-elnode-handler "elnode" "\
Define an Elnode handler function.

This is just like `defun' but it allows Elnode handlers to be
wrapped by other HTTP implementations.

\(fn NAME ARGLIST &rest BODY)" nil (quote macro))

(put 'define-elnode-handler 'lisp-indent-function 'defun)

(autoload 'elnode-init "elnode" "\
Bootstraps the elnode environment when the Lisp is loaded.

It's useful to have elnode start automatically... on Lisp
load.  If the variable `elnode-init-port' is set then this
function will launch a server on it.

The server is started with `elnode-hostpath-default-handler' as
the handler and listening on `elnode-init-host'

\(fn)" t nil)

(defvar elnode-do-init 't "\
Should elnode start a server on load?

The server that is started is controlled by more elnode
customizations.

`elnode-hostpath-default-table' defines the mappings from
hostpath regexs to handler functions. By default elnode ships
with this customization setup to serve the document root defined
in `elnode-webserver-docroot', which by default is ~/public_html.")

(custom-autoload 'elnode-do-init "elnode" t)

(eval-after-load 'elnode (if (and (boundp 'elnode-do-init) elnode-do-init (or (not (boundp 'elnode--inited)) (not elnode--inited))) (progn (elnode-init) (setq elnode--inited nil))))

;;;***

;;;### (autoloads (elnode-wikiserver-test elnode-wikiserver-wikiroot)
;;;;;;  "elnode-wiki" "elnode-wiki.el" (20736 22420))
;;; Generated autoloads from elnode-wiki.el

(defconst elnode-wikiserver-wikiroot-default (expand-file-name (concat elnode-config-directory "wiki/")) "\
The default location of the wiki root.

This is used to detect whether elnode needs to create this
directory or not.")

(defvar elnode-wikiserver-wikiroot elnode-wikiserver-wikiroot-default "\
The root for the Elnode wiki files.

This is where elnode-wikiserver serves wiki files from.")

(custom-autoload 'elnode-wikiserver-wikiroot "elnode-wiki" t)

(autoload 'elnode-wikiserver-test "elnode-wiki" "\
Test whether we should serve Wiki or not.

\(fn)" nil nil)

(define-elnode-handler elnode-wikiserver (httpcon) "Serve Wiki pages from `elnode-wikiserver-wikiroot'.\n\nHTTPCON is the request.\n\nThe Wiki server is only available if the `creole' package is\nprovided. Otherwise it will just error." (if (not (elnode-wikiserver-test)) (elnode-send-500 httpcon "The Emacs feature 'creole is required.") (elnode-wiki--setup) (elnode-wiki-handler httpcon elnode-wikiserver-wikiroot)))

;;;***

;;;### (autoloads nil nil ("elnode-pkg.el" "elnode-rle.el") (20736
;;;;;;  22420 525229))

;;;***

(provide 'elnode-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; elnode-autoloads.el ends here
