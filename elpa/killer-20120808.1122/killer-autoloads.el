;;; killer-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "killer" "killer.el" (0 0 0 0))
;;; Generated autoloads from killer.el

(autoload 'kill-char "killer" "\
Kill the following n characters.

\(fn ARG)" t nil)

(autoload 'backward-kill-char "killer" "\
Kill the previous n characters.

\(fn ARG)" t nil)

(autoload 'kill-or-delete-char "killer" "\
Kill or delete following n characters.

\(fn ARG)" t nil)

(autoload 'backward-kill-or-delete-char "killer" "\
Kill or delete previous n characters.

\(fn ARG)" t nil)

(autoload 'backward-kill-or-delete-char-untabify "killer" "\
Kill or delete previous n characters.

\(fn ARG &optional KILLP)" t nil)

(autoload 'backward-word-or-wspace "killer" "\
Move backward over word or whitespace.
Move backward until end of word or if point is surrounded by
whitespace move to the end of the next word.  With argument,
always move by that many words.

\(fn &optional ARG)" t nil)

(autoload 'forward-word-or-wspace "killer" "\
Move forward over word or whitespace.
Move forward until end of word or if point is surrounded by
whitespace move to the end of the previous word.  With argument,
always move by that many words.

\(fn &optional ARG)" t nil)

(autoload 'backward-delete-whitespace "killer" "\
Delete all spaces and tabs before point.

\(fn)" t nil)

(autoload 'forward-delete-whitespace "killer" "\
Delete all spaces and tabs after point.

\(fn)" t nil)

(autoload 'backward-kill-whitespace "killer" "\
Kill all spaces and tabs before point.

\(fn)" t nil)

(autoload 'forward-kill-whitespace "killer" "\
Kill all spaces and tabs after point.

\(fn)" t nil)

(autoload 'backward-kill-word-or-wspace "killer" "\
Kill characters backward until encountering the end of a word.
If point is surrounded by whitespace kill to the end of the
preciding word.  With argument, always kill that many words.

\(fn &optional ARG)" t nil)

(autoload 'kill-word-or-wspace "killer" "\
Kill characters forward until encountering the end of a word.
If point is surrounded by whitespace kill to the beginning of the
following word.  With argument, always kill that many words.

\(fn &optional ARG)" t nil)

(autoload 'backward-kill-line "killer" "\
Kills the text before point on the current line.
With prefix argument, kill backward n lines from point.  With
negative prefix arguments kill n lines forward.  Don't do this;
use `kill-line' instead.

\(fn &optional ARG)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; killer-autoloads.el ends here
