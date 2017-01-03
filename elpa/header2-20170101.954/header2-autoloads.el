;;; header2-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "header2" "header2.el" (0 0 0 0))
;;; Generated autoloads from header2.el

(autoload 'make-header "header2" "\
Insert (mode-dependent) header comment at beginning of file.
A header is composed of a mode line, a body, and an end line.  The body is
constructed by calling the functions in `make-header-hook'.  The mode line
and end lines start and terminate block comments.  The body lines continue
the comment.

\(fn)" t nil)

(autoload 'make-revision "header2" "\
Prepare for a new history revision.  Insert history line if inexistant.

\(fn)" t nil)

(autoload 'make-divider "header2" "\
Insert a comment divider line: the comment start, filler, and end.
The width is `fill-column', by default.  With a numeric prefix arg,
use that as the width, except use at least 4 columns.

\(fn &optional END-COL)" t nil)

(autoload 'make-box-comment "header2" "\
Insert an empty (mode dependent) box comment.
The maxium width is `fill-column', by default.  With a numeric prefix
arg, use that as the maximum width, except use at least 2 + the length
returned by function `header-prefix-string'.

\(fn &optional END-COL)" t nil)

(autoload 'update-file-header "header2" "\
Update file header.
Search the first `header-max' chars in buffer using regexps in
`file-header-update-alist'.  When a match is found, apply the
corresponding function with point located just after the match.
The functions can use `match-beginning' and `match-end' to find
the strings that cause them to be invoked.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "header2" '("update-" "uniquify-list" "header" "delete-and-forget-line" "auto-" "make-" "true-mode-name" "return-to" "register-file-header-action" "section-comment-start" "nonempty-comment-" "file-header-update-alist")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; header2-autoloads.el ends here
