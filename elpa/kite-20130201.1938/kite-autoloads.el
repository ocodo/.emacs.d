;;; kite-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "kite" "kite.el" (0 0 0 0))
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

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "kite" '("kite-")))

;;;***

;;;### (autoloads nil "kite-breakpoint" "kite-breakpoint.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from kite-breakpoint.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "kite-breakpoint" '("kite-")))

;;;***

;;;### (autoloads nil "kite-color" "kite-color.el" (0 0 0 0))
;;; Generated autoloads from kite-color.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "kite-color" '("kite-")))

;;;***

;;;### (autoloads nil "kite-console" "kite-console.el" (0 0 0 0))
;;; Generated autoloads from kite-console.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "kite-console" '("kite-")))

;;;***

;;;### (autoloads nil "kite-debug" "kite-debug.el" (0 0 0 0))
;;; Generated autoloads from kite-debug.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "kite-debug" '("kite-")))

;;;***

;;;### (autoloads nil "kite-dom" "kite-dom.el" (0 0 0 0))
;;; Generated autoloads from kite-dom.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "kite-dom" '("kite-")))

;;;***

;;;### (autoloads nil "kite-dom-css" "kite-dom-css.el" (0 0 0 0))
;;; Generated autoloads from kite-dom-css.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "kite-dom-css" '("kite-")))

;;;***

;;;### (autoloads nil "kite-files" "kite-files.el" (0 0 0 0))
;;; Generated autoloads from kite-files.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "kite-files" '("kite-")))

;;;***

;;;### (autoloads nil "kite-global" "kite-global.el" (0 0 0 0))
;;; Generated autoloads from kite-global.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "kite-global" '("kite-")))

;;;***

;;;### (autoloads nil "kite-load-path" "kite-load-path.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from kite-load-path.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "kite-load-path" '("kite--directory")))

;;;***

;;;### (autoloads nil "kite-memory" "kite-memory.el" (0 0 0 0))
;;; Generated autoloads from kite-memory.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "kite-memory" '("kite-")))

;;;***

;;;### (autoloads nil "kite-modeline" "kite-modeline.el" (0 0 0 0))
;;; Generated autoloads from kite-modeline.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "kite-modeline" '("kite-")))

;;;***

;;;### (autoloads nil "kite-net" "kite-net.el" (0 0 0 0))
;;; Generated autoloads from kite-net.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "kite-net" '("kite-")))

;;;***

;;;### (autoloads nil "kite-object" "kite-object.el" (0 0 0 0))
;;; Generated autoloads from kite-object.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "kite-object" '("kite-")))

;;;***

;;;### (autoloads nil "kite-scratch" "kite-scratch.el" (0 0 0 0))
;;; Generated autoloads from kite-scratch.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "kite-scratch" '("kite-")))

;;;***

;;;### (autoloads nil "kite-sourcemap" "kite-sourcemap.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from kite-sourcemap.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "kite-sourcemap" '("kite-")))

;;;***

;;;### (autoloads nil "kite-util" "kite-util.el" (0 0 0 0))
;;; Generated autoloads from kite-util.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "kite-util" '("kite--")))

;;;***

;;;### (autoloads nil nil ("kite-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; kite-autoloads.el ends here
