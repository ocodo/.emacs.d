;;; ham-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (ham-mode) "ham-mode" "ham-mode.el" (21163 201
;;;;;;  0 0))
;;; Generated autoloads from ham-mode.el

(autoload 'ham-mode "ham-mode" "\
Html As Markdown. Transparently edit an html file using markdown.

When this mode is activated in an html file, the buffer is
converted to markdown and you may edit at will, but the file is
still saved as html behind the scenes.

To have it activate automatically on html files, do something like:
  (add-to-list 'auto-mode-alist '(\".*\\\\.html\\\\'\" . ham-mode))

Initial conversion uses the `html-to-markdown-this-buffer'
command (handled entirely in elisp by this package :-D).

Subsequent conversions (after every save) are handled by the
markdown executable (which needs to be installed on your system).
See `ham-mode-markdown-command' and `ham-mode--save-as-html' on
how to customize this part.

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("ham-mode-pkg.el") (21163 201 270322 0))

;;;***

(provide 'ham-mode-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; ham-mode-autoloads.el ends here
