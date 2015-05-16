;;; avy-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "avy" "avy.el" (21847 19813 327401 0))
;;; Generated autoloads from avy.el

(autoload 'avy-goto-char "avy" "\
Read one char and jump to it.
The window scope is determined by `avy-all-windows' (ARG negates it).

\(fn &optional ARG)" t nil)

(autoload 'avy-goto-char-2 "avy" "\
Read two consecutive chars and jump to the first one.
The window scope is determined by `avy-all-windows' (ARG negates it).

\(fn &optional ARG)" t nil)

(autoload 'avy-isearch "avy" "\
Jump to one of the current isearch candidates.

\(fn)" t nil)

(autoload 'avy-goto-word-0 "avy" "\
Jump to a word start.
The window scope is determined by `avy-all-windows' (ARG negates it).

\(fn ARG)" t nil)

(autoload 'avy-goto-word-1 "avy" "\
Read one char at word start and jump there.
The window scope is determined by `avy-all-windows' (ARG negates it).

\(fn &optional ARG)" t nil)

(autoload 'avy-goto-subword-0 "avy" "\
Jump to a word or subword start.

The window scope is determined by `avy-all-windows' (ARG negates it).

When PREDICATE is non-nil it's a function of zero parameters that
should return true.

\(fn &optional ARG PREDICATE)" t nil)

(autoload 'avy-goto-subword-1 "avy" "\
Prompt for a subword start char and jump there.
The window scope is determined by `avy-all-windows' (ARG negates it).
The case is ignored.

\(fn &optional ARG)" t nil)

(autoload 'avy-goto-line "avy" "\
Jump to a line start in current buffer.
The window scope is determined by `avy-all-windows' (ARG negates it).

\(fn &optional ARG)" t nil)

(autoload 'avy-copy-line "avy" "\
Copy a selected line above the current line.
ARG lines can be used.

\(fn ARG)" t nil)

(autoload 'avy-move-line "avy" "\
Move a selected line above the current line.
ARG lines can be used.

\(fn ARG)" t nil)

(autoload 'avy-copy-region "avy" "\
Select two lines and copy the text between them here.

\(fn)" t nil)

(autoload 'avy-setup-default "avy" "\
Setup the default shortcuts.

\(fn)" nil nil)

(autoload 'avy-goto-char-timer "avy" "\
Read one or two consecutive chars and jump to the first one.
The window scope is determined by `avy-all-windows' (ARG negates it).

\(fn &optional ARG)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; avy-autoloads.el ends here
