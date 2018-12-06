;;; grizzl-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "grizzl" "grizzl.el" (0 0 0 0))
;;; Generated autoloads from grizzl.el

(autoload 'grizzl-make-index "grizzl" "\
Makes an index from the list STRINGS for use with `grizzl-search'.

If :PROGRESS-FN is given as a keyword argument, it is called repeatedly
with integers N and TOTAL.

If :CASE-SENSITIVE is specified as a non-nil keyword argument, the index
will be created case-sensitive, otherwise it will be case-insensitive.

\(fn STRINGS &rest OPTIONS)" nil nil)

(autoload 'grizzl-search "grizzl" "\
Fuzzy searches for TERM in INDEX prepared with `grizzl-make-index'.

OLD-RESULT may be specified as an existing search result to increment from.
The result can be read with `grizzl-result-strings'.

\(fn TERM INDEX &optional OLD-RESULT)" nil nil)

(autoload 'grizzl-result-count "grizzl" "\
Returns the number of matches present in RESULT.

\(fn RESULT)" nil nil)

(autoload 'grizzl-result-strings "grizzl" "\
Returns the ordered list of matched strings in RESULT, using INDEX.

If the :START option is specified, results are read from the given offset.
If the :END option is specified, up to :END results are returned.

\(fn RESULT INDEX &rest OPTIONS)" nil nil)

(autoload 'grizzl-completing-read "grizzl" "\
Performs a completing-read in the minibuffer using INDEX to fuzzy search.
Each key pressed in the minibuffer filters down the list of matches.

\(fn PROMPT INDEX)" nil nil)

(autoload 'grizzl-selected-result "grizzl" "\
Get the selected string from INDEX in a `grizzl-completing-read'.

\(fn INDEX)" nil nil)

(autoload 'grizzl-set-selection+1 "grizzl" "\
Move the selection up one row in `grizzl-completing-read'.

\(fn)" t nil)

(autoload 'grizzl-set-selection-1 "grizzl" "\
Move the selection down one row in `grizzl-completing-read'.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "grizzl" '("grizzl-" "*grizzl-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; grizzl-autoloads.el ends here
