;;; elgrep-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "elgrep" "elgrep.el" (0 0 0 0))
;;; Generated autoloads from elgrep.el

(autoload 'elgrep-menu "elgrep" "\
Present a menu with most of the parameters for `elgrep'.
Reset the menu entries if RESET is non-nil.
You can adjust the parameters there and start `elgrep'.

\(fn &optional RESET)" t nil)

(autoload 'elgrep "elgrep" "\
In path DIR grep files with name matching FILE-NAME-RE for text matching RE.
This is done via Emacs Lisp (no dependence on external grep).
Return list of filematches.

Each filematch is a cons (file . matchdata).
file is the file name.
matchdata is a list of matches.
Each match is a list of sub-matches.
Each submatch is a plist of :match, :context, :line,
:linestart, :beg and :end.

OPTIONS is a plist
Flags:

:abs absolute file names
t: full absolute file names;
nil: (default) file names relative to `default-directory'
of the last visited buffer

:interactive
t: call as interactive

:c-beg context begin (line beginning)
Lines before match defaults to 0. Can also be a regular expression.
Then this re is searched for in backward-direction
starting at the current elgrep-match.

:c-end context end (line end)
Lines behind match defaults to 0
Then this re is searched for in forward-direction
starting at the current elgrep-match.

:c-op
Context operation gets beginning and end position of context as arguments.
Defaults to `buffer-substring-no-properties'.

:recursive
t: also grep recursively subdirectories in dir
\(also if called interactively with prefix arg)

:formatter
Formatting function to call for each match
if called interactively with non-nil RE.
Inputs: format string \"%s:%d:%s
\", file-name, line number,

:exclude-file-re
Regular expression matching the files that should not be grepped.

:dir-re
Regular expression matching the directories
that should be entered in recursive grep.
Defaults to \"\".

:exclude-dir-re
Regular expression matching the directories
that should not be entered in recursive grep.
If this is the empty string no directories are excluded.
Defaults to \"^\\.\".

:case-fold-search
Ignore case if non-nil.
Defaults to the value of `case-fold-search'.

:search-fun
Function to search forward for occurences of RE
with the same arguments as `re-search-forward'.
It is actually not required that REGEXP is a regular expression.
t just must be be understood by :search-fun.
Defaults to `re-search-forward'.

:keep-elgrep-buffer
Keep buffer <*elgrep*> even when there are no matches.

:no-header
Avoid descriptive header into <*elgrep*> buffer.

:async
Asynchronous search (experimental).

:mindepth Minimal depth. Defaults to 0.

:maxdepth Maximal depth. Defaults to the value of `most-positive-fixnum'.

:depth Internal. Should not be used.

\(fn DIR FILE-NAME-RE RE &rest OPTIONS)" t nil)

(easy-menu-add-item global-map '("menu-bar" "tools") ["Search Files (Elgrep)..." elgrep-menu t] "grep")

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "elgrep" '("elgrep-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; elgrep-autoloads.el ends here
