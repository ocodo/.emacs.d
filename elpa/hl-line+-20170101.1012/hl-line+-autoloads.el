;;; hl-line+-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "hl-line+" "hl-line+.el" (0 0 0 0))
;;; Generated autoloads from hl-line+.el

(defface hl-line '((t (:background "SlateGray3"))) "\
*Face to use for `hl-line-face'." :group (quote hl-line))

(defvar hl-line-flash-show-period 1 "\
*Number of seconds for `hl-line-flash' to highlight the line.")

(custom-autoload 'hl-line-flash-show-period "hl-line+" t)

(defvar hl-line-inhibit-highlighting-for-modes nil "\
*Modes where highlighting is inhibited for `hl-line-highlight-now'.
A list of `major-mode' values (symbols).")

(custom-autoload 'hl-line-inhibit-highlighting-for-modes "hl-line+" t)

(defvar hl-line-overlay-priority -50 "\
*Priority to use for `hl-line-overlay' and `global-hl-line-overlay'.
A higher priority can make the hl-line highlighting appear on top of
other overlays that might exist.")

(custom-autoload 'hl-line-overlay-priority "hl-line+" t)

(defalias 'toggle-hl-line-when-idle 'hl-line-toggle-when-idle)

(autoload 'hl-line-toggle-when-idle "hl-line+" "\
Turn on or off using `global-hl-line-mode' when Emacs is idle.
When on, use `global-hl-line-mode' whenever Emacs is idle.
With prefix argument, turn on if ARG > 0; else turn off.

In Lisp code, non-nil optional second arg MSGP means display a message
showing the new value.

\(fn &optional ARG MSGP)" t nil)

(autoload 'hl-line-when-idle-interval "hl-line+" "\
Set wait until using `global-hl-line-mode' when Emacs is idle.
Whenever Emacs is idle for this many seconds, `global-hl-line-mode'
will be turned on.

To turn on or off using `global-hl-line-mode' when idle,
use `\\[toggle-hl-line-when-idle].

\(fn SECS)" t nil)

(defalias 'flash-line-highlight 'hl-line-flash)

(autoload 'hl-line-flash "hl-line+" "\
Highlight the current line for `hl-line-flash-show-period' seconds.
With a prefix argument, highlight for that many seconds.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "hl-line+" '("hl-line-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; hl-line+-autoloads.el ends here
