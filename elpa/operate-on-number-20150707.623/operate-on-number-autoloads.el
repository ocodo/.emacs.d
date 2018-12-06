;;; operate-on-number-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "operate-on-number" "operate-on-number.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from operate-on-number.el

(autoload 'find-number-at-point "operate-on-number" "\
Search the current line till EOL for a number.
If a pure number is found, move point to the end of the number
and return the value.  Raise an error otherwise.

\(fn)" t nil)

(autoload 'apply-operation-to-number-at-point "operate-on-number" "\
Apply an operation specified by KEY on a number at point.

If called interactively, use the last key input as KEY.

If the operation requires an additional operand, it is taken from
one of the following sources in the order named:

1. Prefix argument if given

2. Value read from keyboard if READ-ARGS is non-nil or the :read
   property is non-nil

3. Default argument predefined in `operate-on-number-at-point-alist'

\(fn &optional KEY READ-ARGS)" t nil)

(autoload 'operate-on-number-at-point "operate-on-number" "\
Operate on number at point.

The kind of operation to perform is specified by the following
key typed.

An optional number ARG becomes a counter operand to the number at
point for the operation if applicable.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "operate-on-number" '("apply-to-number-at-point" "oon--" "operate-on-number-at-point-alist")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; operate-on-number-autoloads.el ends here
