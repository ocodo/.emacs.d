;;; markdown-toc-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "markdown-toc" "markdown-toc.el" (0 0 0 0))
;;; Generated autoloads from markdown-toc.el

(autoload 'markdown-toc-version "markdown-toc" "\
Markdown-toc version.

\(fn)" t nil)

(autoload 'markdown-toc-generate-toc "markdown-toc" "\
Generate a TOC for markdown file at current point.
Deletes any previous TOC.
If called interactively with prefix arg REPLACE-TOC-P, replaces previous TOC.

\(fn &optional REPLACE-TOC-P)" t nil)

(autoload 'markdown-toc-generate-or-refresh-toc "markdown-toc" "\
Generate a TOC for markdown file at current point or refreshes an already generated TOC.

\(fn)" t nil)

(autoload 'markdown-toc-refresh-toc "markdown-toc" "\
Refreshes an already generated TOC.

\(fn)" t nil)

(autoload 'markdown-toc-delete-toc "markdown-toc" "\
Deletes a previously generated TOC.

\(fn)" t nil)

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
