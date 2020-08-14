;;; kill-or-bury-alive-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "kill-or-bury-alive" "kill-or-bury-alive.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from kill-or-bury-alive.el

(defvar kill-or-bury-alive-must-die-list nil "\
List of buffer designators for buffers that always should be killed.

See description of `kill-or-bury-alive--buffer-match' for
information about the concept of buffer designators.

This variable is used by `kill-or-bury-alive' function.")

(custom-autoload 'kill-or-bury-alive-must-die-list "kill-or-bury-alive" t)

(defvar kill-or-bury-alive-killing-function-alist nil "\
AList that maps buffer designators to functions that should kill them.

See description of `kill-or-bury-alive--buffer-match' for
information about the concept of buffer designators.

This variable is used by `kill-or-bury-alive' and
`kill-or-bury-alive-purge-buffers'.

You can use `kill-or-bury-alive-kill-with' to add elements to this alist.")

(custom-autoload 'kill-or-bury-alive-killing-function-alist "kill-or-bury-alive" t)

(defvar kill-or-bury-alive-long-lasting-list '("^\\*scratch\\*$" "^\\*Messages\\*$" "^ ?\\*git-credential-cache--daemon\\*$" erc-mode) "\
List of buffer designators for buffers that should not be purged.

See description of `kill-or-bury-alive--buffer-match' for
information about the concept of buffer designators.

This variable is used by `kill-or-bury-alive-purge-buffers'.")

(custom-autoload 'kill-or-bury-alive-long-lasting-list "kill-or-bury-alive" t)

(autoload 'kill-or-bury-alive-kill-with "kill-or-bury-alive" "\
Kill buffers selected by BUFFER-DESIGNATOR with KILLING-FUNCTION.

See description of `kill-or-bury-alive--buffer-match' for
information about the concept of buffer designators.

Normally, KILLING-FUNCTION should be able to take one argument:
buffer object.  However, you can use a function that operates on
current buffer and doesn't take any arguments.  Just pass non-NIL
SIMPLE argument and KILLING-FUNCTION will be wrapped as needed
automatically.

\(fn BUFFER-DESIGNATOR KILLING-FUNCTION &optional SIMPLE)" nil nil)

(autoload 'kill-or-bury-alive "kill-or-bury-alive" "\
Kill or bury the current buffer.

This is a universal killing mechanism.  When argument ARG is
given and it's not NIL, kill current buffer.  Otherwise behavior
of this command varies.  If current buffer matches a buffer
designator listed in `kill-or-bury-alive-must-die-list', kill it
immediately, otherwise just bury it.

You can specify how to kill various kinds of buffers, see
`kill-or-bury-alive-killing-function-alist' for more information.
Buffers are killed with `kill-or-bury-alive-killing-function' by
default.

\(fn &optional ARG)" t nil)

(autoload 'kill-or-bury-alive-purge-buffers "kill-or-bury-alive" "\
Kill all buffers except for long lasting ones.

Long lasting buffers are specified in `kill-or-bury-alive-long-lasting-list'.

If `kill-or-bury-alive-base-buffer' is not NIL, switch to buffer
with that name after purging and delete all other windows.

When ARG is given and it's not NIL, ask to confirm killing of
every buffer.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "kill-or-bury-alive" '("kill-or-bury-alive-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; kill-or-bury-alive-autoloads.el ends here
