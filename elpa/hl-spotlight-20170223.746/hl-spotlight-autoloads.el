;;; hl-spotlight-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "hl-spotlight" "hl-spotlight.el" (0 0 0 0))
;;; Generated autoloads from hl-spotlight.el

(defface hl-spotlight '((t :inherit highlight)) "\
*Face for the spotlight in Hl-Line-Window mode." :group (quote hl-line))

(defvar hl-spotlight-height 2 "\
*Number of lines to highlight, before and after the current line.")

(custom-autoload 'hl-spotlight-height "hl-spotlight" t)

(defvar hl-spotlight-keep-centered-flag t "\
*Non-nil means keep the cursor and spotlight centered in the window.
This has no effect unless library `centered-cursor-mode' is available.")

(custom-autoload 'hl-spotlight-keep-centered-flag "hl-spotlight" t)

(defvar hl-spotlight-scan-period 1.5 "\
*Number of seconds to wait before moving cursor to next line.
Set this to nil if you do not want the cursor to automatically scan.")

(custom-autoload 'hl-spotlight-scan-period "hl-spotlight" t)

(autoload 'hl-spotlight-enlarge "hl-spotlight" "\
Enlarge the hl-line spotlight by N lines.
N is the numeric prefix arg (one, by default).
A negative prefix arg shrinks the spotlight.
The spotlight is used by `hl-spotlight-mode' and
`global-hl-spotlight-mode'.

\(fn N)" t nil)

(autoload 'hl-spotlight-mode "hl-spotlight" "\
Buffer-local minor mode to highlight lines surrounding point.
With ARG, turn Hl-Spotlight mode on if ARG is positive, off otherwise.

Hl-Spotlight mode uses Hl-Line mode.  Whenever Hl-Spotlight mode is on
in the current buffer, its overlay is used by Hl-Line mode, which
means that face `hl-spotlight' and option `hl-spotlight-height' are
used; face `hl-line' is not used.

Turn the spotlight on and off by using toggle command
`hl-spotlight-mode'.  After turning Hl-Spotlight mode on, command
`hl-line-mode' also toggles the spotlight on and off, but without
turning off Hl-Spotlight mode.  To return to the normal behavior of
`hl-line-mode', you must turn off Hl-Spotlight mode.  Turning off
Hl-Spotlight mode also turns off Hl-Line mode.

\(fn &optional ARG)" t nil)

(defvar global-hl-spotlight-mode nil "\
Non-nil if Global Hl-Spotlight mode is enabled.
See the `global-hl-spotlight-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-hl-spotlight-mode'.")

(custom-autoload 'global-hl-spotlight-mode "hl-spotlight" nil)

(autoload 'global-hl-spotlight-mode "hl-spotlight" "\
Global minor mode to highlight lines around point in current window.
With ARG, turn Global-Hl-Spotlight mode on if ARG is positive, off
otherwise.

See `hl-spotlight-mode'.  The interaction between
`global-hl-spotlight-mode' and `global-hl-line-mode' is similar to
that between `hl-spotlight-mode' and `hl-line-mode'.

\(fn &optional ARG)" t nil)

(autoload 'hl-spotlight-scan "hl-spotlight" "\
Scan the buffer, moving the cursor down automatically.
Every `hl-spotlight-scan-period' seconds, move the cursor down one
line or the number of lines specified by a prefix arg.  Scanning
starts at point.

With a plain prefix arg (`C-u'), stop a scan already in progess.

With a numeric prefix arg, scan down that many lines.
A negative prefix arg means scan up, not down.
With `C-u C-u', scan down the height of a full spotlight.

\(fn ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "hl-spotlight" '("hl-spotlight-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; hl-spotlight-autoloads.el ends here
