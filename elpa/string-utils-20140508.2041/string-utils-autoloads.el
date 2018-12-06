;;; string-utils-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "string-utils" "string-utils.el" (0 0 0 0))
;;; Generated autoloads from string-utils.el

(autoload 'string-utils-stringify-anything "string-utils" "\
Coerce any object OBJ into a string.

Contrary to usual conventions, return the empty string for nil.

Sequences are flattened down to atoms and joined with string
SEPARATOR, which defaults to a single space.  Cyclic lists
may give unpredictable results (similar to `format') unless
list-utils.el is installed.

When INTS-ARE-CHARS is non-nil, interpret positive integers in
OBJ as characters.

Optional RECORD-SEPARATOR is a string (defaulting to the value of
SEPARATOR) which delimits end-of-record for paired data types
such as hash tables.

This is not a pretty-printer for OBJ, but a way to look at
the *contents* of OBJ (so much as is possible) as if it was
an ordinary string.

\(fn OBJ &optional SEPARATOR INTS-ARE-CHARS RECORD-SEPARATOR)" nil nil)

(autoload 'string-utils-has-darkspace-p "string-utils" "\
Test whether OBJ, when coerced to a string, has any non-whitespace characters.

Returns the position of the first non-whitespace character
on success.

If optional WHITESPACE-TYPE is 'ascii or t, use an ASCII-only
definition of whitespace characters.  If WHITESPACE-TYPE is
'syntax, is the definition of whitespace from the current
`syntax-table'.  Otherwise, use a broad, Unicode-aware
definition of whitespace from `string-utils-whitespace'.

\(fn OBJ &optional WHITESPACE-TYPE)" nil nil)

(autoload 'string-utils-has-whitespace-p "string-utils" "\
Test whether OBJ, when coerced to a string, has any whitespace characters.

Returns the position of the first whitespace character on
success.

If optional WHITESPACE-TYPE is 'ascii or t, use an ASCII-only
definition of whitespace characters.  If WHITESPACE-TYPE is
'syntax, is the definition of whitespace from the current
`syntax-table'.  Otherwise, use a broad, Unicode-aware
definition of whitespace from `string-utils-whitespace'.

\(fn OBJ &optional WHITESPACE-TYPE)" nil nil)

(autoload 'string-utils-trim-whitespace "string-utils" "\
Return STR-VAL with leading and trailing whitespace removed.

If optional WHITESPACE-TYPE is 'ascii or t, use an ASCII-only
definition of whitespace characters.  If WHITESPACE-TYPE is
'syntax, is the definition of whitespace from the current
`syntax-table'.  Otherwise, use a broad, Unicode-aware
definition of whitespace from `string-utils-whitespace'.

If optional MULTI-LINE is set, trim spaces at starts and
ends of all lines throughout STR-VAL.

\(fn STR-VAL &optional WHITESPACE-TYPE MULTI-LINE)" nil nil)

(autoload 'string-utils-compress-whitespace "string-utils" "\
Return STR-VAL with all contiguous whitespace compressed to SEPARATOR.

The default value of SEPARATOR is a single space: \" \".

If optional WHITESPACE-TYPE is 'ascii or t, use an ASCII-only
definition of whitespace characters.  If WHITESPACE-TYPE is
'syntax, is the definition of whitespace from the current
`syntax-table'.  Otherwise, use a broad, Unicode-aware
definition of whitespace from `string-utils-whitespace'.

\(fn STR-VAL &optional WHITESPACE-TYPE SEPARATOR)" nil nil)

(autoload 'string-utils-string-repeat "string-utils" "\
Return a new string formed by repeating STR-VAL, N times.

STR-VAL may be of any length.

\(fn STR-VAL N)" nil nil)

(autoload 'string-utils-escape-double-quotes "string-utils" "\
Return STR-VAL with every double-quote escaped with backslash.

\(fn STR-VAL)" nil nil)

(autoload 'string-utils-quotemeta "string-utils" "\
Return STR-VAL with all non-word characters escaped with backslash.

This is more vigorous than `shell-quote-argument'.

\(fn STR-VAL)" nil nil)

(autoload 'string-utils-pad "string-utils" "\
Pad STR-VAL to WIDTH.

Optional MODE defaults to 'right, but may be 'left, 'center, or
an integer.

When MODE is 'left, padding characters are prepended.  When MODE
is 'center, padding characters are both appended and prepended so
that STR-VAL is centered within WIDTH.

When MODE is a positive integer, the behavior is fixed-position
padding.  Similar to 'center, padding may be added on the right
and on the left.  Exactly MODE-many padding characters are
added on the left before padding to the full WIDTH on the right.
When MODE is a negative integer, the behavior is the same, except
that MODE fixes the right-side padding.

Optional CHAR sets the padding character (defaults to space).

Optional THROW-ERROR throws an error if the length of STR-VAL
already exceeds WIDTH, or if the fixed-position padding requested
would cause the result to exceed WIDTH.  When THROW-ERROR is not
set (the default), a best-attempt result is always returned.

Tabs are expanded to spaces according to the value of
`tab-width'.

Returns a padded copy of string STR-VAL.

\(fn STR-VAL WIDTH &optional MODE CHAR THROW-ERROR)" nil nil)

(autoload 'string-utils-pad-list "string-utils" "\
Pad each member of STR-LIST to match the longest width.

ADDITIONAL-WIDTH sets a relative amount to pad beyond the longest
length.

TARGET-WIDTH sets an absolute target width, causing maximum
string length and ADDITIONAL-WIDTH to be ignored.

Optional MODE, CHAR, and THROW-ERROR are as for `string-utils-pad'.
Fixed-position MODE will attempt to pad all entries consistently,
based on any adjustments made to the longest member of STR-LIST.

Tabs are expanded to spaces according to the value of
`tab-width'.

Returns padded STR-LIST.

\(fn STR-LIST &optional ADDITIONAL-WIDTH TARGET-WIDTH MODE CHAR THROW-ERROR)" nil nil)

(autoload 'string-utils-propertize-fillin "string-utils" "\
Return a copy of STR-VAL with text properties added, without overriding.

Works exactly like `propertize', except that (character-by-character)
already existing properties are respected.

STR-VAL and PROPERTIES are treated as documented for the STRING
and PROPERTIES arguments to `propertize'.

\(fn STR-VAL &rest PROPERTIES)" nil nil)

(autoload 'string-utils-plural-ending "string-utils" "\
Return \"s\" or \"\", depending on whether NUM requires a plural in English.

Intended to be used in a format string as follows:

    (message \"%s item%s deleted\" del-counter (string-utils-plural-ending del-counter))

\(fn NUM)" nil nil)

(autoload 'string-utils-squeeze-filename "string-utils" "\
Intelligibly squeeze file-name or buffer-name NAME to fit within MAXLEN.

When shortening file or buffer names for presentation to human
readers, it is often preferable not to truncate the ends, but to
remove leading or middle portions of the string.

This function keeps basename intact, and (failing that) the
beginning and end of the basename, so that a shortened file or
buffer name is more identifiable to a human reader.

The heuristic

   1.  Works equally for file names or buffer names.

   2.  Applies abbreviations to file names such as \"~\" for home
       directory.

   3.  Selectively removes the longest leading directory
       components from a path, preferring to keep the rightmost
       components, leaving a single ellipsis where any number of
       path elements were removed.

   4.  Shortens the basename of NAME if needed, preserving the
       meaningful file extension.

The string returned is as long as MAXLEN or shorter.

When PATH-REMOVAL is non nil, it is permitted to shorten a
pathname by removing the directory components completely,
substituting no ellipsis.

ELLIPSIS is a string inserted wherever characters were removed.
It defaults to the UCS character \"Horizontal Ellipsis\", or
\"...\" if extended characters are not displayable.

If NO-TAIL is set, do not preserve the trailing letters of
a filename unless there is a dotted extension.

\(fn NAME MAXLEN &optional PATH-REMOVAL ELLIPSIS NO-TAIL)" nil nil)

(autoload 'string-utils-squeeze-url "string-utils" "\
Intelligibly squeeze string URL to fit within MAXLEN.

Fit URL within MAXLEN for presentation to a human reader.
Follows rules similar to `string-utils-squeeze-filename'.

ELLIPSIS is a string inserted wherever characters were removed.
It defaults to the UCS character \"Horizontal Ellipsis\", or
\"...\" if extended characters are not displayable.

\(fn URL MAXLEN &optional ELLIPSIS)" nil nil)

(autoload 'string-utils-split "string-utils" "\
Like `split-string', with additional options.

STRING, SEPARATORS, and OMIT-NULLS are as documented at `split-string'.

INCLUDE-SEPARATORS is currently unimplemented.

When RESPECT-ESCAPES is set, STRING is not split where the
separator is escaped with backslash.  This currently has the
limitation that SEPARATORS must be an explicit string rather than
a regular expression.

\(fn STRING &optional SEPARATORS OMIT-NULLS INCLUDE-SEPARATORS RESPECT-ESCAPES)" nil nil)

(autoload 'string-utils-truncate-to "string-utils" "\
Truncate STRING to MAXLEN.

The returned value is of length MAXLEN or less, including
ELLIPSIS.

ELLIPSIS is a string inserted wherever characters were removed.
It defaults to the UCS character \"Horizontal Ellipsis\", or
\"...\" if extended characters are not displayable.

\(fn STR-VAL MAXLEN &optional ELLIPSIS)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "string-utils" '("string-utils-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; string-utils-autoloads.el ends here
