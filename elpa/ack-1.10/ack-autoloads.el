;;; ack-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "ack" "ack.el" (0 0 0 0))
;;; Generated autoloads from ack.el

(autoload 'ack "ack" "\
Run ack using COMMAND-ARGS and collect output in a buffer.
When called interactively, the value of DIRECTORY is provided by
`ack-default-directory-function'.

The following keys are available while reading from the
minibuffer:

\\{ack-minibuffer-local-map}

\(fn COMMAND-ARGS &optional DIRECTORY)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ack" '("ack-")))

;;;***

;;;### (autoloads nil "pcmpl-ack" "pcmpl-ack.el" (0 0 0 0))
;;; Generated autoloads from pcmpl-ack.el

(autoload 'pcomplete/ack "pcmpl-ack" "\
Completion for the `ack' command.
Start an argument with `-' to complete short options and `--' for
long options." nil nil)

(defalias 'pcomplete/ack-grep 'pcomplete/ack)

(autoload 'pcomplete/ag "pcmpl-ack" "\
Completion for the `ag' command." nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pcmpl-ack" '("pcmpl-ack-")))

;;;***

;;;### (autoloads nil nil ("ack-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; ack-autoloads.el ends here
