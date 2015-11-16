;;; kite-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "kite" "kite.el" (22089 54921 0 0))
;;; Generated autoloads from kite.el

(autoload 'kite-console "kite" "\
Go to the Kite Console buffer for the session specified by
PREFIX.  Session and buffer are created as needed.  An existing
session is reused if possible, unless a prefix argument of (4) is
given in which case a new session is established.  With a prefix
of (16), Kite will prompt for remote host name and port.  With a
numeric prefix (1 or higher), Kite will reuse the Nth session,
where sessions are counted in the order in which they were
created.

\(fn PREFIX)" t nil)

(autoload 'kite-debug "kite" "\
Go to the Kite Debug buffer for the session specified by
PREFIX.  Session and buffer are created as needed.  An existing
session is reused if possible, unless a prefix argument of (4) is
given in which case a new session is established.  With a prefix
of (16), Kite will prompt for remote host name and port.  With a
numeric prefix (1 or higher), Kite will reuse the Nth session,
where sessions are counted in the order in which they were
created.

\(fn PREFIX)" t nil)

(autoload 'kite-dom "kite" "\
Go to the Kite DOM buffer for the session specified by
PREFIX.  Session and buffer are created as needed.  An existing
session is reused if possible, unless a prefix argument of (4) is
given in which case a new session is established.  With a prefix
of (16), Kite will prompt for remote host name and port.  With a
numeric prefix (1 or higher), Kite will reuse the Nth session,
where sessions are counted in the order in which they were
created.

\(fn PREFIX)" t nil)

(autoload 'kite-network "kite" "\
Go to the Kite Network buffer for the session specified by
PREFIX.  Session and buffer are created as needed.  An existing
session is reused if possible, unless a prefix argument of (4) is
given in which case a new session is established.  With a prefix
of (16), Kite will prompt for remote host name and port.  With a
numeric prefix (1 or higher), Kite will reuse the Nth session,
where sessions are counted in the order in which they were
created.

\(fn PREFIX)" t nil)

(autoload 'kite-scratch "kite" "\
Go to the Kite scratch buffer for the session specified by
PREFIX.  Session and buffer are created as needed.  An existing
session is reused if possible, unless a prefix argument of (4) is
given in which case a new session is established.  With a prefix
of (16), Kite will prompt for remote host name and port.  With a
numeric prefix (1 or higher), Kite will reuse the Nth session,
where sessions are counted in the order in which they were
created.

\(fn PREFIX)" t nil)

(autoload 'kite-timeline "kite" "\
Go to the Kite Timeline buffer for the session specified by
PREFIX.  Session and buffer are created as needed.  An existing
session is reused if possible, unless a prefix argument of (4) is
given in which case a new session is established.  With a prefix
of (16), Kite will prompt for remote host name and port.  With a
numeric prefix (1 or higher), Kite will reuse the Nth session,
where sessions are counted in the order in which they were
created.

\(fn PREFIX)" t nil)

(autoload 'kite-javascript-profiler "kite" "\
Go to the Kite JavaScript Profiler buffer for the session
specified by PREFIX.  Session and buffer are created as needed.
An existing session is reused if possible, unless a prefix
argument of (4) is given in which case a new session is
established.  With a prefix of (16), Kite will prompt for remote
host name and port.  With a numeric prefix (1 or higher), Kite
will reuse the Nth session, where sessions are counted in the
order in which they were created.

\(fn PREFIX)" t nil)

(autoload 'kite-css-profiler "kite" "\
Go to the Kite CSS Profiler buffer for the session specified
by PREFIX.  Session and buffer are created as needed.  An
existing session is reused if possible, unless a prefix argument
of (4) is given in which case a new session is established.  With
a prefix of (16), Kite will prompt for remote host name and port.
With a numeric prefix (1 or higher), Kite will reuse the Nth
session, where sessions are counted in the order in which they
were created.

\(fn PREFIX)" t nil)

(autoload 'kite-heap-profiler "kite" "\
Go to the Kite Heap Profiler buffer for the session specified
by PREFIX.  Session and buffer are created as needed.  An
existing session is reused if possible, unless a prefix argument
of (4) is given in which case a new session is established.  With
a prefix of (16), Kite will prompt for remote host name and port.
With a numeric prefix (1 or higher), Kite will reuse the Nth
session, where sessions are counted in the order in which they
were created.

\(fn PREFIX)" t nil)

;;;***

;;;### (autoloads nil nil ("kite-breakpoint.el" "kite-color.el" "kite-console.el"
;;;;;;  "kite-debug.el" "kite-dom-css.el" "kite-dom.el" "kite-files.el"
;;;;;;  "kite-global.el" "kite-load-path.el" "kite-memory.el" "kite-modeline.el"
;;;;;;  "kite-net.el" "kite-object.el" "kite-pkg.el" "kite-scratch.el"
;;;;;;  "kite-sourcemap.el" "kite-util.el") (22089 54922 382557 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; kite-autoloads.el ends here
