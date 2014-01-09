;;; lusty-explorer-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (lusty-launch-dired lusty-select-current-name lusty-select-match
;;;;;;  lusty-open-this lusty-highlight-previous-column lusty-highlight-next-column
;;;;;;  lusty-highlight-previous lusty-highlight-next lusty-explorer-mode
;;;;;;  lusty-buffer-explorer lusty-file-explorer) "lusty-explorer"
;;;;;;  "lusty-explorer.el" (20838 20704 0 0))
;;; Generated autoloads from lusty-explorer.el

(autoload 'lusty-file-explorer "lusty-explorer" "\
Launch the file/directory mode of LustyExplorer.

\(fn)" t nil)

(autoload 'lusty-buffer-explorer "lusty-explorer" "\
Launch the buffer mode of LustyExplorer.

\(fn)" t nil)

(defvar lusty-explorer-mode nil "\
Non-nil if Lusty-Explorer mode is enabled.
See the command `lusty-explorer-mode' for a description of this minor mode.")

(custom-autoload 'lusty-explorer-mode "lusty-explorer" nil)

(autoload 'lusty-explorer-mode "lusty-explorer" "\
Toggle Lusty Explorer mode.
With a prefix argument ARG, enable Lusty Explorer mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil.

Lusty Explorer mode is a global minor mode that enables switching
between buffers and finding files using substrings, fuzzy matching,
and recency information.

\(fn &optional ARG)" t nil)

(autoload 'lusty-highlight-next "lusty-explorer" "\
Highlight the next match in *Lusty-Matches*.

\(fn)" t nil)

(autoload 'lusty-highlight-previous "lusty-explorer" "\
Highlight the previous match in *Lusty-Matches*.

\(fn)" t nil)

(autoload 'lusty-highlight-next-column "lusty-explorer" "\
Highlight the next column in *Lusty-Matches*.

\(fn)" t nil)

(autoload 'lusty-highlight-previous-column "lusty-explorer" "\
Highlight the previous column in *Lusty-Matches*.

\(fn)" t nil)

(autoload 'lusty-open-this "lusty-explorer" "\
Open the given file/directory/buffer, creating it if not already present.

\(fn)" t nil)

(autoload 'lusty-select-match "lusty-explorer" "\
Activate the highlighted match in *Lusty-Matches* - recurse if dir, open if file/buffer.

\(fn)" t nil)

(autoload 'lusty-select-current-name "lusty-explorer" "\
Open the given file/buffer or create a new buffer with the current name.

\(fn)" t nil)

(autoload 'lusty-launch-dired "lusty-explorer" "\
Launch dired at the current directory.

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("lusty-explorer-pkg.el") (20838 20704
;;;;;;  225051 0))

;;;***

(provide 'lusty-explorer-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; lusty-explorer-autoloads.el ends here
