;;; ack-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (ack) "ack" "ack.el" (21015 10681 0 0))
;;; Generated autoloads from ack.el

(autoload 'ack "ack" "\
Run ack using COMMAND-ARGS and collect output in a buffer.
When called interactively, the value of DIRECTORY is provided by
`ack-default-directory-function'.

The following keys are available while reading from the
minibuffer:

\\{ack-minibuffer-local-map}

\(fn COMMAND-ARGS &optional DIRECTORY)" t nil)

;;;***

;;;### (autoloads (pcomplete/ack) "pcmpl-ack" "pcmpl-ack.el" (21015
;;;;;;  10681 0 0))
;;; Generated autoloads from pcmpl-ack.el

(autoload 'pcomplete/ack "pcmpl-ack" "\
Completion for the `ack' command.
Start an argument with '-' to complete short options and '--' for
long options.

\(fn)" nil nil)

(defalias 'pcomplete/ack-grep 'pcomplete/ack)

;;;***

;;;### (autoloads nil nil ("ack-pkg.el") (21015 10681 741398 0))

;;;***

(provide 'ack-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; ack-autoloads.el ends here
