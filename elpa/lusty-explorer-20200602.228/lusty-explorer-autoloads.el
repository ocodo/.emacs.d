;;; lusty-explorer-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "lusty-explorer" "lusty-explorer.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from lusty-explorer.el

(autoload 'lusty-file-explorer "lusty-explorer" "\
Launch the file/directory mode of LustyExplorer." t nil)

(autoload 'lusty-buffer-explorer "lusty-explorer" "\
Launch the buffer mode of LustyExplorer." t nil)

(defvar lusty-explorer-mode nil "\
Non-nil if Lusty-Explorer mode is enabled.
See the `lusty-explorer-mode' command
for a description of this minor mode.")

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
Highlight the next match in *Lusty-Matches*." t nil)

(autoload 'lusty-highlight-previous "lusty-explorer" "\
Highlight the previous match in *Lusty-Matches*." t nil)

(autoload 'lusty-highlight-next-column "lusty-explorer" "\
Highlight the next column in *Lusty-Matches*." t nil)

(autoload 'lusty-highlight-previous-column "lusty-explorer" "\
Highlight the previous column in *Lusty-Matches*." t nil)

(autoload 'lusty-open-this "lusty-explorer" "\
Open the given file/directory/buffer, creating it if not already present." t nil)

(autoload 'lusty-select-match "lusty-explorer" "\
Activate the highlighted match in *Lusty-Matches* - recurse if dir, open if file/buffer." t nil)

(autoload 'lusty-yank "lusty-explorer" "\
A `yank' variant that adds some intuitive behavior in the case where
`default-directory' is at the root (\"/\") of a remote TRAMP connection and the
pasted path is absolute (i.e. has a leading \"/\"). The pasted path is
assumed to be on the remote filesystem rather than the local (that being the
default behavior, generally less useful).

\(fn ARG)" t nil)

(autoload 'lusty-select-current-name "lusty-explorer" "\
Open the given file/buffer or create a new buffer with the current name." t nil)

(autoload 'lusty-launch-dired "lusty-explorer" "\
Launch dired at the current directory." t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lusty-explorer" '("LM-score" "lusty-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; lusty-explorer-autoloads.el ends here
