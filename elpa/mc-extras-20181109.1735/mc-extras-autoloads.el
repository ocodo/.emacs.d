;;; mc-extras-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "mc-cua" "mc-cua.el" (0 0 0 0))
;;; Generated autoloads from mc-cua.el

(autoload 'mc/cua-rectangle-to-multiple-cursors "mc-cua" "\
Turn CUA rectangle mode into multiple-cursors mode, keeping insert positions and selections.

\(fn)" t nil)

(autoload 'mc/cua-rectangle-setup "mc-cua" "\
Enable interaction between multiple cursors and CUA rectangle copy & paste.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "mc-cua" '("mc/cua-saved-kill-ring")))

;;;***

;;;### (autoloads nil "mc-freeze" "mc-freeze.el" (0 0 0 0))
;;; Generated autoloads from mc-freeze.el

(autoload 'mc/freeze-fake-cursors "mc-freeze" "\
Freeze fake cursors for later reactivation.

With ARG or when there is no fake cursor, create a fake cursor at
point before freezing fake cursors.

\(fn &optional ARG)" t nil)

(autoload 'mc/unfreeze-fake-cursors "mc-freeze" "\
Unfreeze frozen fake cursors.

\(fn)" t nil)

(autoload 'mc/freeze-fake-cursors-dwim "mc-freeze" "\
Freeze or unfreeze fake cursors depending on the current state.

With ARG, always create a fake cursor at point then freeze fake
cursors.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "mc-freeze" '("mc-freeze--")))

;;;***

;;;### (autoloads nil "mc-mark-extras" "mc-mark-extras.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from mc-mark-extras.el

(autoload 'mc/mark-next-sexps "mc-mark-extras" "\
Mark next ARG sexps.

\(fn ARG)" t nil)

(autoload 'mc/mark-previous-sexps "mc-mark-extras" "\
Mark previous ARG sexps.

\(fn ARG)" t nil)

(autoload 'mc/mark-all-below "mc-mark-extras" "\
Mark lines below until the cursor hits a line shorter than the current column position.

\(fn)" t nil)

(autoload 'mc/mark-all-above "mc-mark-extras" "\
Mark lines above until the cursor hits a line shorter than the current column position.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "mc-mark-extras" '("mc/mark-sexps")))

;;;***

;;;### (autoloads nil "mc-move" "mc-move.el" (0 0 0 0))
;;; Generated autoloads from mc-move.el

(autoload 'mc/move-to-column "mc-move" "\
Move every cursor to column COLUMN.
If COLUMN is omitted, move every fake cursor to the same column as the real cursor.

\(fn COLUMN)" t nil)

(autoload 'mc/compare-chars "mc-move" "\
Compare the character at point with that at each fake cursor, and move forward as far as they all match.
With an optional argument, move backwards by calling `mc/compare-chars-backward'.
This command pushes the mark before moving cursors.

\(fn &optional ARG)" t nil)

(autoload 'mc/compare-chars-forward "mc-move" "\
Compare the character at point with that at each fake cursor, and move forward as far as they all match.
This command pushes the mark before moving cursors.

\(fn)" t nil)

(autoload 'mc/compare-chars-backward "mc-move" "\
Backwards version of `mc/compare-chars-forward'.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "mc-rect" "mc-rect.el" (0 0 0 0))
;;; Generated autoloads from mc-rect.el

(autoload 'mc/rect-rectangle-to-multiple-cursors "mc-rect" "\
Turn rectangle-mark-mode into multiple-cursors mode, keeping selections.

\(fn START END)" t nil)

;;;***

;;;### (autoloads nil "mc-remove" "mc-remove.el" (0 0 0 0))
;;; Generated autoloads from mc-remove.el

(autoload 'mc/remove-current-cursor "mc-remove" "\
Remove the current cursor by replacing the next fake cursor with the real cursor.

\(fn)" t nil)

(autoload 'mc/remove-duplicated-cursors "mc-remove" "\
Remove duplicated fake cursors, including ones that overlap the real cursor.

\(fn)" t nil)

(autoload 'mc/remove-cursors-at-bol "mc-remove" "\
Remove cursors at BOL, either fake or real.

\(fn)" t nil)

(autoload 'mc/remove-cursors-at-eol "mc-remove" "\
Remove cursors at EOL, either fake or real.

\(fn)" t nil)

(autoload 'mc/remove-cursors-on-blank-lines "mc-remove" "\
Remove cursors on blank lines, either fake or real.

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("mc-extras-pkg.el" "mc-extras.el") (0
;;;;;;  0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; mc-extras-autoloads.el ends here
