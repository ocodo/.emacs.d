;;; which-key-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "which-key" "which-key.el" (22507 31279 797225
;;;;;;  204000))
;;; Generated autoloads from which-key.el

(defvar which-key-mode nil "\
Non-nil if Which-Key mode is enabled.
See the `which-key-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `which-key-mode'.")

(custom-autoload 'which-key-mode "which-key" nil)

(autoload 'which-key-mode "which-key" "\
Toggle which-key-mode.

\(fn &optional ARG)" t nil)

(autoload 'which-key-setup-side-window-right "which-key" "\
Apply suggested settings for side-window that opens on right.

\(fn)" t nil)

(autoload 'which-key-setup-side-window-right-bottom "which-key" "\
Apply suggested settings for side-window that opens on right
if there is space and the bottom otherwise.

\(fn)" t nil)

(autoload 'which-key-setup-side-window-bottom "which-key" "\
Apply suggested settings for side-window that opens on
bottom.

\(fn)" t nil)

(autoload 'which-key-setup-minibuffer "which-key" "\
Apply suggested settings for minibuffer.

\(fn)" t nil)

(autoload 'which-key-add-key-based-replacements "which-key" "\
Replace the description of KEY-SEQUENCE with REPLACEMENT.
Both KEY-SEQUENCE and REPLACEMENT should be strings.  For Example,

\(which-key-add-key-based-replacements \"C-x 1\" \"maximize\")

MORE allows you to specifcy additional KEY REPL pairs.  All
replacements are added to
`which-key-key-based-description-replacement-alist'.

\(fn KEY-SEQUENCE REPLACEMENT &rest MORE)" nil nil)

(autoload 'which-key-add-major-mode-key-based-replacements "which-key" "\
Functions like `which-key-add-key-based-replacements'.
The difference is that MODE specifies the `major-mode' that must
be active for KEY-SEQUENCE and REPLACEMENT (MORE contains
addition KEY-SEQUENCE REPLACEMENT pairs) to apply.

\(fn MODE KEY-SEQUENCE REPLACEMENT &rest MORE)" nil nil)

(autoload 'which-key-add-prefix-title "which-key" "\
Deprecated in favor of `which-key-declare-prefixes'.

Add title for KEY-SEQ-STR given by TITLE. FORCE, if non-nil, will
add the new title even if one already exists. KEY-SEQ-STR should
be a key sequence string suitable for `kbd' and TITLE should be a
string.

\(fn KEY-SEQ-STR TITLE &optional FORCE)" nil nil)

(autoload 'which-key-declare-prefixes "which-key" "\
Name the KEY-SEQUENCE prefix NAME.
KEY-SEQUENCE should be a string, acceptable to `kbd'. NAME can be
a string or a cons cell of two strings. In the first case, the
string is used as both the name and the title (the title is
displayed in the echo area only). For Example,

\(which-key-declare-prefixes \"C-x 8\" \"unicode\")

or

\(which-key-declare-prefixes \"C-x 8\" (\"unicode\" . \"Unicode Chararcters\"))

MORE allows you to specifcy additional KEY-SEQUENCE NAME pairs.
All names are added to `which-key-prefix-names-alist' and titles
to `which-key-prefix-title-alist'.

\(fn KEY-SEQUENCE NAME &rest MORE)" nil nil)

(autoload 'which-key-declare-prefixes-for-mode "which-key" "\
Functions like `which-key-declare-prefixes'.
The difference is that MODE specifies the `major-mode' that must
be active for KEY-SEQUENCE and NAME (MORE contains
addition KEY-SEQUENCE NAME pairs) to apply.

\(fn MODE KEY-SEQUENCE NAME &rest MORE)" nil nil)

(autoload 'which-key-reload-key-sequence "which-key" "\
Simulate entering the key sequence KEY-SEQ.
KEY-SEQ should be a list of events as produced by
`listify-key-sequence'. Any prefix arguments that were used are
reapplied to the new key sequence.

\(fn KEY-SEQ)" nil nil)

(autoload 'which-key-show-standard-help "which-key" "\
Call the command in `which-key--prefix-help-cmd-backup'.
Usually this is `describe-prefix-bindings'.

\(fn)" t nil)

(autoload 'which-key-show-next-page-no-cycle "which-key" "\
Show next page of keys unless on the last page, in which case
call `which-key-show-standard-help'.

\(fn)" t nil)

(autoload 'which-key-show-previous-page-no-cycle "which-key" "\
Show previous page of keys unless on the first page, in which
case do nothing.

\(fn)" t nil)

(autoload 'which-key-show-next-page-cycle "which-key" "\
Show the next page of keys, cycling from end to beginning
after last page.

\(fn)" t nil)

(autoload 'which-key-show-previous-page-cycle "which-key" "\
Show the previous page of keys, cycling from beginning to end
after first page.

\(fn)" t nil)

(autoload 'which-key-show-top-level "which-key" "\
Show top-level bindings.

\(fn)" t nil)

(autoload 'which-key-undo-key "which-key" "\
Undo last keypress and force which-key update.

\(fn)" t nil)

(autoload 'which-key-C-h-dispatch "which-key" "\
Dispatch C-h commands by looking up key in
`which-key-C-h-map'. This command is always accessible (from any
prefix) if `which-key-use-C-h-commands' is non nil.

\(fn)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; which-key-autoloads.el ends here
