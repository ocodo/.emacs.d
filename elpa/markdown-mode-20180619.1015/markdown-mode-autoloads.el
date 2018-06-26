;;; markdown-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "markdown-mode" "markdown-mode.el" (23344 44850
;;;;;;  763929 299000))
;;; Generated autoloads from markdown-mode.el

(autoload 'markdown-mode "markdown-mode" "\
Major mode for editing Markdown files.

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode) t)

(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode) t)

(autoload 'gfm-mode "markdown-mode" "\
Major mode for editing GitHub Flavored Markdown files.

\(fn)" t nil)

(autoload 'markdown-view-mode "markdown-mode" "\
Major mode for viewing Markdown content.

\(fn)" t nil)

(autoload 'gfm-view-mode "markdown-mode" "\
Major mode for viewing GitHub Flavored Markdown content.

\(fn)" t nil)

(autoload 'markdown-live-preview-mode "markdown-mode" "\
Toggle native previewing on save for a specific markdown file.

\(fn &optional ARG)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; markdown-mode-autoloads.el ends here
