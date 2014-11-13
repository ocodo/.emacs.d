;;; ace-window-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "ace-window" "ace-window.el" (21604 41027 765235
;;;;;;  56000))
;;; Generated autoloads from ace-window.el

(autoload 'aw-list-visual-area "ace-window" "\
Forward to `ace-jump-list-visual-area', removing invisible frames.

\(fn)" nil nil)

(autoload 'aw-generic "ace-window" "\
Create a window-manipulating function.
MODE-LINE is a string to display while a window is being selected.
HANDLER is a function that takes a window argument.

\(fn MODE-LINE HANDLER)" nil t)

(autoload 'ace-select-window "ace-window" "\


\(fn)" t nil)

(autoload 'ace-delete-window "ace-window" "\


\(fn)" t nil)

(autoload 'ace-swap-window "ace-window" "\


\(fn)" t nil)

(autoload 'ace-window "ace-window" "\
Select a window with `ace-jump-mode'and perform an action based on prefix ARG.
Variations are described below.

By default, behaves like extended `other-window'.

Prefixed with one \\[universal-argument], does a swap between selected window
 and current window, so that the selected buffer moves to current window (and
 current buffer moves to selected window).

Prefixed with two \\[universal-argument]'s, deletes the selected window.

\(fn ARG)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; ace-window-autoloads.el ends here
