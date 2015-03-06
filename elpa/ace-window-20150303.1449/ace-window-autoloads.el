;;; ace-window-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "ace-window" "ace-window.el" (21753 17598 367401
;;;;;;  0))
;;; Generated autoloads from ace-window.el

(autoload 'ace-select-window "ace-window" "\
Ace select window.

\(fn)" t nil)

(autoload 'ace-delete-window "ace-window" "\
Ace delete window.

\(fn)" t nil)

(autoload 'ace-swap-window "ace-window" "\
Ace swap window.

\(fn)" t nil)

(autoload 'ace-maximize-window "ace-window" "\
Ace maximize window.

\(fn)" t nil)

(autoload 'ace-window "ace-window" "\
Select a window.
Perform an action based on ARG described below.

By default, behaves like extended `other-window'.

Prefixed with one \\[universal-argument], does a swap between the
selected window and the current window, so that the selected
buffer moves to current window (and current buffer moves to
selected window).

Prefixed with two \\[universal-argument]'s, deletes the selected
window.

\(fn ARG)" t nil)

;;;***

;;;### (autoloads nil "avy-jump" "avy-jump.el" (21753 17598 371401
;;;;;;  0))
;;; Generated autoloads from avy-jump.el

(autoload 'avi-goto-char "avy-jump" "\
Read one char and jump to it in current window.

\(fn)" t nil)

(autoload 'avi-goto-char-2 "avy-jump" "\
Read two chars and jump to them in current window.

\(fn)" t nil)

(autoload 'avi-isearch "avy-jump" "\
Jump to one of the current isearch candidates.

\(fn)" t nil)

(autoload 'avi-goto-word-0 "avy-jump" "\
Jump to a word start in current window.

\(fn)" t nil)

(autoload 'avi-goto-word-1 "avy-jump" "\
Jump to a word start in current window.
Read one char with which the word should start.

\(fn)" t nil)

(autoload 'avi-goto-line "avy-jump" "\
Jump to a line start in current buffer.

\(fn)" t nil)

(autoload 'avi-copy-line "avy-jump" "\
Copy a selected line above the current line.
ARG lines can be used.

\(fn ARG)" t nil)

(autoload 'avi-move-line "avy-jump" "\
Move a selected line above the current line.
ARG lines can be used.

\(fn ARG)" t nil)

(autoload 'avi-copy-region "avy-jump" "\
Select two lines and copy the text between them here.

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("ace-window-pkg.el" "avy.el") (21753 17598
;;;;;;  386018 219000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; ace-window-autoloads.el ends here
