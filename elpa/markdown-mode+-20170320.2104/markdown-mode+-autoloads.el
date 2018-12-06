;;; markdown-mode+-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "markdown-mode+" "markdown-mode+.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from markdown-mode+.el

(autoload 'markdown-export-latex "markdown-mode+" "\
Output the Markdown file as LaTeX.

\(fn)" t nil)

(autoload 'markdown-export-pdf "markdown-mode+" "\
Output the Markdown file as LaTeX.

\(fn)" t nil)

(autoload 'markdown-export-pandoc-pdf "markdown-mode+" "\
Output the Markdown file as LaTeX.

\(fn)" t nil)

(autoload 'markdown-code-copy "markdown-mode+" "\
Copy region from BEGIN to END to the clipboard with four spaces indenteded on each line.

Taken from
http://stackoverflow.com/questions/3519244/emacs-command-to-indent-code-by-4-spaces-to-format-for-paste-into-stackoverflow.

\(fn BEGIN END)" t nil)

(autoload 'markdown-copy-rtf "markdown-mode+" "\
Render markdown and copy as RTF.

\(fn)" t nil)

(autoload 'markdown-copy-paste-html "markdown-mode+" "\
Process file with multimarkdown, copy it to the clipboard, and paste in safari's selected textarea.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "markdown-mode+" '("markdown-")))

;;;***

;;;### (autoloads nil nil ("markdown-mode+-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; markdown-mode+-autoloads.el ends here
