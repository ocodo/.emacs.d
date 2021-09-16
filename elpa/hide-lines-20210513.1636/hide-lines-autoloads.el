;;; hide-lines-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "hide-lines" "hide-lines.el" (0 0 0 0))
;;; Generated autoloads from hide-lines.el

(autoload 'hide-lines "hide-lines" "\
Hide lines matching the specified regexp.
With prefix ARG of 4 (\\[universal-argument]) hide lines that do not match the
specified regexp.  With any other prefix arg, reveal all hidden lines.

\(fn &optional ARG)" t nil)

(autoload 'hide-blocks "hide-lines" "\
Hide blocks of lines between matching regexps.
With prefix ARG of 4 (\\[universal-argument]) hide blocks that do not match the
specified regexps.  With any other prefix arg, reveal all hidden blocks.

\(fn &optional ARG)" t nil)

(autoload 'hide-lines-not-matching "hide-lines" "\
Hide lines that don't match the regexp SEARCH-TEXT.

\(fn SEARCH-TEXT)" t nil)

(autoload 'hide-lines-matching "hide-lines" "\
Hide lines matching the regexp SEARCH-TEXT.

\(fn SEARCH-TEXT)" t nil)

(autoload 'hide-blocks-not-matching "hide-lines" "\
Hide text that is not between lines matching START-TEXT and END-TEXT.

\(fn START-TEXT END-TEXT)" t nil)

(autoload 'hide-blocks-matching "hide-lines" "\
Hide text that is between lines matching START-TEXT and END-TEXT.

\(fn START-TEXT END-TEXT)" t nil)

(autoload 'hide-lines-show-all "hide-lines" "\
Unhide all hidden areas." t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "hide-lines" '("hide-lines-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; hide-lines-autoloads.el ends here
