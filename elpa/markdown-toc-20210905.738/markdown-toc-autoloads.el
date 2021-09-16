;;; markdown-toc-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "markdown-toc" "markdown-toc.el" (0 0 0 0))
;;; Generated autoloads from markdown-toc.el

(autoload 'markdown-toc-version "markdown-toc" "\
Markdown-toc version." t nil)

(autoload 'markdown-toc-generate-toc "markdown-toc" "\
Generate a TOC for markdown file at current point.
Deletes any previous TOC.
If called interactively with prefix arg REPLACE-TOC-P, replaces previous TOC.

\(fn &optional REPLACE-TOC-P)" t nil)

(autoload 'markdown-toc-generate-or-refresh-toc "markdown-toc" "\
Generate a TOC for markdown file at current point or refreshes an already generated TOC." t nil)

(autoload 'markdown-toc-refresh-toc "markdown-toc" "\
Refreshes an already generated TOC." t nil)

(autoload 'markdown-toc-delete-toc "markdown-toc" "\
Deletes a previously generated TOC." t nil)

(autoload 'markdown-toc-follow-link-at-point "markdown-toc" "\
On a given toc link, navigate to the current markdown header.
If the toc is misindented (according to markdown-toc-indentation-space`)
or if not on a toc link, this does nothing.
" t nil)

(autoload 'markdown-toc-mode "markdown-toc" "\
Functionality for generating toc in markdown file.
With no argument, the mode is toggled on/off.
Non-nil argument turns mode on.
Nil argument turns mode off.

If called interactively, enable Markdown-Toc mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

Commands:
\\{markdown-toc-mode-map}

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "markdown-toc" '("markdown-")))

;;;***

;;;### (autoloads nil nil ("markdown-toc-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; markdown-toc-autoloads.el ends here
