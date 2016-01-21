;;; igrep-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "igrep" "igrep.el" (22176 24834 0 0))
;;; Generated autoloads from igrep.el

(autoload 'igrep-insinuate "igrep" "\
Define `grep' aliases for the corresponding `igrep' commands.
With a prefix arg, OVERRIDE the current `grep' command definitions.

\(fn &optional OVERRIDE)" t nil)

(autoload 'igrep "igrep" "\
*Run `grep` PROGRAM to match REGEX in FILES.
The output is displayed in the *igrep* buffer, which `\\[next-error]' and
`\\[compile-goto-error]' parse to find each line of matched text.

PROGRAM may be nil, in which case it defaults to `igrep-program'.

REGEX is automatically quoted by `shell-quote-argument'.

FILES is either a file name pattern (automatically quoted by
`shell-quote-wildcard-pattern', then expanded by the `shell-file-name' shell),
or a list of file name patterns.

Optional OPTIONS is also passed to PROGRAM; it defaults to `igrep-options'.

If a prefix argument (`\\[universal-argument]') is given when called interactively,
or if `igrep-read-options' is set, OPTIONS is read from the minibuffer.

If two prefix arguments (`\\[universal-argument] \\[universal-argument]') are given when called interactively,
or if `igrep-read-multiple-files' is set, FILES is read from the minibuffer
multiple times.

If three prefix arguments (`\\[universal-argument] \\[universal-argument] \\[universal-argument]') are given when called interactively,
or if `igrep-read-options' and `igrep-read-multiple-files' are set,
OPTIONS is read and FILES is read multiple times.

If `igrep-find' is non-nil, the directory or directories
containing FILES is recursively searched for files whose name matches
the file name component of FILES (and whose contents match REGEX).

\(fn PROGRAM REGEX FILES &optional OPTIONS)" t nil)

(autoload 'igrep-find "igrep" "\
*Run `grep` via `find`; see `igrep' and `igrep-find'.
All IGREP-ARGS (including prefix arguments, when called interactively)
are handled by `igrep'.

\(fn &rest IGREP-ARGS)" t nil)

(autoload 'igrep-visited-files "igrep" "\
*Run `grep` PROGRAM to match REGEX (with optional OPTIONS) on all visited files.
See `\\[igrep]'.

\(fn PROGRAM REGEX &optional OPTIONS)" t nil)

(autoload 'dired-do-igrep "igrep" "\
*Search the marked (or next prefix ARG) files.
See `\\[igrep]' for a description of PROGRAM, REGEX, and OPTIONS.

\(fn PROGRAM REGEX &optional OPTIONS ARG)" t nil)

(autoload 'dired-do-igrep-find "igrep" "\
*Run `grep` on the marked (or next prefix ARG) directories.
See `\\[igrep]' for a description of PROGRAM, REGEX, and OPTIONS.

\(fn PROGRAM REGEX &optional OPTIONS ARG)" t nil)

(autoload 'Buffer-menu-igrep "igrep" "\
*Run `grep` on the files visited in buffers marked with '>'.
See `\\[igrep]' for a description of PROGRAM, REGEX, and OPTIONS.

\(fn PROGRAM REGEX &optional OPTIONS)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; igrep-autoloads.el ends here
