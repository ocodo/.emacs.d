;;; ack-and-a-half-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (ack-and-a-half-find-file-same ack-and-a-half-find-file
;;;;;;  ack-and-a-half-same ack-and-a-half) "ack-and-a-half" "ack-and-a-half.el"
;;;;;;  (21011 20064 0 0))
;;; Generated autoloads from ack-and-a-half.el

(autoload 'ack-and-a-half "ack-and-a-half" "\
Run ack.
PATTERN is interpreted as a regular expression, iff REGEXP is
non-nil.  If called interactively, the value of REGEXP is
determined by `ack-and-a-half-regexp-search'.  A prefix arg
toggles the behavior.  DIRECTORY is the root directory.  If
called interactively, it is determined by
`ack-and-a-half-project-root-file-patterns'.  The user is only
prompted, if `ack-and-a-half-prompt-for-directory' is set.

\(fn PATTERN &optional REGEXP DIRECTORY)" t nil)

(autoload 'ack-and-a-half-same "ack-and-a-half" "\
Run ack with --type matching the current `major-mode'.
The types of files searched are determined by
`ack-and-a-half-mode-type-alist' and
`ack-and-a-half-mode-extension-alist'.  If no type is configured,
the buffer's file extension is used for the search.  PATTERN is
interpreted as a regular expression, iff REGEXP is non-nil.  If
called interactively, the value of REGEXP is determined by
`ack-and-a-half-regexp-search'.  A prefix arg toggles that value.
DIRECTORY is the directory in which to start searching.  If
called interactively, it is determined by
`ack-and-a-half-project-root-file-patterns`.  The user is only
prompted, if `ack-and-a-half-prompt-for-directory' is set.`

\(fn PATTERN &optional REGEXP DIRECTORY)" t nil)

(autoload 'ack-and-a-half-find-file "ack-and-a-half" "\
Prompt to find a file found by ack in DIRECTORY.

\(fn &optional DIRECTORY)" t nil)

(autoload 'ack-and-a-half-find-file-same "ack-and-a-half" "\
Prompt to find a file found by ack in DIRECTORY.

\(fn &optional DIRECTORY)" t nil)

;;;***

;;;### (autoloads nil nil ("ack-and-a-half-pkg.el") (21011 20064
;;;;;;  399330 0))

;;;***

(provide 'ack-and-a-half-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; ack-and-a-half-autoloads.el ends here
