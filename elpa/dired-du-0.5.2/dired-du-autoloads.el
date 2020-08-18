;;; dired-du-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "dired-du" "dired-du.el" (0 0 0 0))
;;; Generated autoloads from dired-du.el

(autoload 'dired-du-mode "dired-du" "\
Toggle dired-du mode.
Interactively with no argument, this command toggles the mode.
A positive prefix argument enables the mode, any other prefix
argument disables it.  From Lisp, argument omitted or nil enables
the mode, `toggle' toggles the state.

If called interactively, enable Dired-Du mode if ARG is positive,
and disable it if ARG is zero or negative.  If called from Lisp,
also enable the mode if ARG is omitted or nil, and toggle it if
ARG is `toggle'; disable the mode otherwise.

Show the recursive size of directories in Dired buffers.
Once this mode is enabled, every new Dired buffer displays
recursive dir sizes.
The directory size is obtained with `dired-du-used-space-program'.

Note that obtaining the recursive size of all the directories
in a Dired buffer might be slow; thus, it may significantly delay
the time to display a new Dired buffer.

Instead of enabling `dired-du-mode' by default in all Dired buffers
you might prefer to use this mode as a convenient interface to
the `du' program: just enable it in the current Dired buffer,
and disable it once you have finished checking the used space.

\(fn &optional ARG)" t nil)

(autoload 'dired-du-count-sizes "dired-du" "\
Count sizes of files marked with MARK.
If MARK evaluates nil, then use `dired-marker-char'.

Optional arg ALL-MARKS, if non-nil, then accept all mark characters.

Optional arg INCLUDE-DIRS, if non-nil, include the recursive size of the
marked directories.
If called interactively with a prefix, then prompt for previous
args.  Otherwise, all optional arguments but INCLUDE-DIRS are nil, and
INCLUDE-DIRS is set to variable `dired-du-mode'.

Directories '.' '..' are not special: if they are marked, then return
their recursive size.

\(fn MARK &optional ALL-MARKS INCLUDE-DIRS)" t nil)

(autoload 'dired-du-insert-marked-dirs "dired-du" "\
Insert all marked subdirectories." t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "dired-du" '("dired-du-")))

;;;***

;;;### (autoloads nil nil ("dired-du-pkg.el" "dired-du-tests.el")
;;;;;;  (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; dired-du-autoloads.el ends here
