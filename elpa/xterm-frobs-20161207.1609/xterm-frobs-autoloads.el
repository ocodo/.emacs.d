;;; xterm-frobs-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "xterm-frobs" "xterm-frobs.el" (22613 12708
;;;;;;  676311 136000))
;;; Generated autoloads from xterm-frobs.el

(autoload 'xterm-iconify "xterm-frobs" "\
Minimize (iconify) xterm window.

\(fn)" t nil)

(autoload 'xterm-deiconify "xterm-frobs" "\
Restore (deiconify) xterm window.

\(fn)" t nil)

(autoload 'xterm-set-font "xterm-frobs" "\
Set the font of the xterm window to FONT.
When called interactively, prompt for the name of the font to use.

This function is used to change the font of the xterm window in which a
tty-mode emacs is running.  It should also work if emacs is running under
`screen' in an xterm window.

Use \\[set-default-font] if this emacs is using the window system directly.

\(fn FONT-NAME)" t nil)

(autoload 'xterm-set-icon-title "xterm-frobs" "\
Set the title in the icon for this xterm window to TITLE.
This does not change the title of the corresponding window.

\(fn TITLE)" t nil)

(autoload 'xterm-set-window-title "xterm-frobs" "\
Set the title for xterm window to TITLE.
This does not change the title in the corresponding icon.

\(fn TITLE)" t nil)

(autoload 'xterm-set-all-titles "xterm-frobs" "\
Set the title for xterm window and corresponding icon to TITLE.

\(fn TITLE)" t nil)

(autoload 'xterm-set-background-color "xterm-frobs" "\


\(fn COLOR)" t nil)

(autoload 'xterm-set-foreground-color "xterm-frobs" "\


\(fn COLOR)" t nil)

(autoload 'xterm-set-cursor-color "xterm-frobs" "\


\(fn COLOR)" t nil)

(autoload 'xterm-set-mouse-foreground-color "xterm-frobs" "\


\(fn COLOR)" t nil)

(autoload 'xterm-set-mouse-background-color "xterm-frobs" "\


\(fn COLOR)" t nil)

(autoload 'xterm-set-highlight-color "xterm-frobs" "\


\(fn COLOR)" t nil)

(autoload 'xterm-reverse-video "xterm-frobs" "\
Set xterm to reverse video mode.
For monochrome xterms, this is white foreground on black background.
For xterms which support color, this has the effect of swapping the
foreground and background colors, whatever they may be.

The effect of this command and \\[xterm-normal-video] may be exchanged
if the XTerm*reverseVideo resource property is set to True.

\(fn)" t nil)

(autoload 'xterm-normal-video "xterm-frobs" "\
Set xterm to normal (i.e. non-reverse) video mode.
For monochrome xterms, this is black foreground on white background.
For xterms which support color, this has the effect of restoring the
original foreground and background colors, whatever they may be.

The effect of this command and \\[xterm-reverse-video] may be exchanged
if the XTerm*reverseVideo resource property is set to True.

\(fn)" t nil)

(autoload 'xterm-sync-emacs-colors "xterm-frobs" "\
Query xterm for color palette and define color list for Emacs.

\(fn)" t nil)

(autoload 'xterm-print-formatted-color-alist "xterm-frobs" "\
Create a pretty-printed table of the current xterm color map.
This table is inserted into a new buffer which can be saved to a file and
reloaded later.

\(fn &optional COLOR-ALIST)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; xterm-frobs-autoloads.el ends here
