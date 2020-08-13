;;; xterm-color-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "xterm-color" "xterm-color.el" (0 0 0 0))
;;; Generated autoloads from xterm-color.el

(autoload 'xterm-color-filter-strip "xterm-color" "\
Translate ANSI color sequences in STRING into text properties.
Return new STRING with text properties applied.

In order to get maximum performance, this function strips text properties
if they are present in STRING.

\(fn STRING)" nil nil)

(autoload 'xterm-color-filter "xterm-color" "\
Translate ANSI color sequences in STRING into text properties.
Return new STRING with text properties applied.

This function checks if `xterm-color-preserve-properties' is non-nil
and only calls `xterm-color-filter-strip' on substrings that do not
have text properties applied (passing through the rest unmodified).
Preserving properties in this fashion is not very robust as there may
be situations where text properties are applied on ANSI data, which
will desync the state machine.

Preserving properties works ok with and is really meant for eshell.

This can be inserted into `comint-preoutput-filter-functions'.

\(fn STRING)" nil nil)

(autoload 'xterm-color-256 "xterm-color" "\


\(fn COLOR)" nil nil)

(autoload 'xterm-color-colorize-buffer "xterm-color" "\
Apply `xterm-color-filter' to current buffer, and replace its contents.
Colors are applied using 'face, unless font-lock-mode is active, in
which case 'font-lock-face is used. Operation with font-lock mode active
is not recommended.

If USE-OVERLAYS is non-nil, colors are applied to the buffer using overlays
instead of text properties. A C-u prefix arg causes overlays to be used.

\(fn &optional USE-OVERLAYS)" t nil)

(autoload 'xterm-color-clear-cache "xterm-color" "\
Clear xterm color face attribute cache.
You may want to call this if you change `xterm-color-names' or
`xterm-color-names-bright' at runtime and you want to see the changes
take place in a pre-existing buffer that has had xterm-color initialized.

Since the cache is buffer-local and created on-demand when needed, this has no
effect when called from a buffer that does not have a cache." t nil)

(autoload 'xterm-color-test "xterm-color" "\
Create, display and render a new buffer containing ANSI control sequences." t nil)

(autoload 'xterm-color-test-raw "xterm-color" "\
Create and display a new buffer containing ANSI SGR control sequences.
ANSI sequences are not processed. One can use a different Emacs package,
such as ansi-color.el to do so. This is really meant to be used for easy
comparisons/benchmarks with libraries that offer similar functionality." t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "xterm-color" '("+xterm-color--table-256+" "xterm-color-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; xterm-color-autoloads.el ends here
