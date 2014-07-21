;;; ace-window-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "ace-window" "ace-window.el" (21452 39495 526176
;;;;;;  999000))
;;; Generated autoloads from ace-window.el

(autoload 'aw-list-visual-area "ace-window" "\
Forward to `ace-jump-list-visual-area', removing invisible frames.

\(fn)" nil nil)

(autoload 'aw-generic "ace-window" "\
Create a window-manipulating function.
MODE-LINE is a string to display while a window is being selected.
HANDLER is a function that takes a window argument.

\(fn MODE-LINE HANDLER)" nil t)

(defalias 'ace-select-window (aw-generic " Ace - Window" aw-switch-to-window) "\
Ace select window.")

(defalias 'ace-delete-window (aw-generic " Ace - Delete Window" aw-delete-window) "\
Ace delete window.")

(defalias 'ace-swap-window (aw-generic " Ace - Swap Window" aw-swap-window) "\
Ace swap window.")

(autoload 'ace-window "ace-window" "\
Ace jump to window and perform an action based on prefix ARG.
- with no arg: select window
- with one arg: swap window
- with double arg: delete window

\(fn ARG)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; ace-window-autoloads.el ends here
