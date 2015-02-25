;;; mc-extras-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "mc-compare" "mc-compare.el" (21741 24584 703401
;;;;;;  0))
;;; Generated autoloads from mc-compare.el

(autoload 'mc/compare-chars "mc-compare" "\
Compare the character at point with that at each fake cursor, and move forward as far as they all match.
With an optional argument, move backwards by calling `mc/compare-chars-backward'.
This command pushes the mark before moving cursors.

\(fn &optional ARG)" t nil)

(autoload 'mc/compare-chars-forward "mc-compare" "\
Compare the character at point with that at each fake cursor, and move forward as far as they all match.
This command pushes the mark before moving cursors.

\(fn)" t nil)

(autoload 'mc/compare-chars-backward "mc-compare" "\
Backwards version of `mc/compare-chars-forward'.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "mc-cua" "mc-cua.el" (21741 24584 695401 0))
;;; Generated autoloads from mc-cua.el

(autoload 'mc/cua-rectangle-to-multiple-cursors "mc-cua" "\
Turn CUA rectangle mode into multiple-cursors mode, keeping insert positions and selections.

\(fn)" t nil)

(autoload 'mc/cua-rectangle-setup "mc-cua" "\
Enable interaction between multiple cursors and CUA rectangle copy & paste.

\(fn)" nil nil)

;;;***

;;;### (autoloads nil "mc-freeze" "mc-freeze.el" (21741 24584 707401
;;;;;;  0))
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

;;;***

;;;### (autoloads nil "mc-rect" "mc-rect.el" (21741 24584 711401
;;;;;;  0))
;;; Generated autoloads from mc-rect.el

(autoload 'mc/rect-rectangle-to-multiple-cursors "mc-rect" "\
Turn rectangle-mark-mode into multiple-cursors mode, keeping selections.

\(fn START END)" t nil)

;;;***

;;;### (autoloads nil "mc-remove" "mc-remove.el" (21741 24584 711401
;;;;;;  0))
;;; Generated autoloads from mc-remove.el

(autoload 'mc/remove-current-cursor "mc-remove" "\
Remove the current cursor by replacing the next fake cursor with the real cursor.

\(fn)" t nil)

(autoload 'mc/remove-duplicated-cursors "mc-remove" "\
Remove duplicated fake cursors, including ones that overlap the real cursor.

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("mc-extras-pkg.el" "mc-extras.el") (21741
;;;;;;  24584 731866 768000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; mc-extras-autoloads.el ends here
