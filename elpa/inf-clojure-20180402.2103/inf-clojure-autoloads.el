;;; inf-clojure-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "inf-clojure" "inf-clojure.el" (0 0 0 0))
;;; Generated autoloads from inf-clojure.el

(autoload 'inf-clojure-minor-mode "inf-clojure" "\
Minor mode for interacting with the inferior Clojure process buffer.

The following commands are available:

\\{inf-clojure-minor-mode-map}

\(fn &optional ARG)" t nil)

(autoload 'inf-clojure "inf-clojure" "\
Run an inferior Clojure process, input and output via buffer `*inf-clojure*'.
If there is a process already running in `*inf-clojure*', just switch
to that buffer.
With argument, allows you to edit the CMD used to launch
it (default is value of `inf-clojure-*-cmd').  Runs the hooks
from `inf-clojure-mode-hook' (after the `comint-mode-hook' is
run).
\(Type \\[describe-mode] in the process buffer for a list of commands.)

\(fn CMD)" t nil)

(autoload 'inf-clojure-connect "inf-clojure" "\
Connect to a running socket-repl via `inf-clojure'.
HOST is the host the process is running on, PORT is where it's listening.

\(fn HOST PORT)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "inf-clojure" '("inf-clojure-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; inf-clojure-autoloads.el ends here
