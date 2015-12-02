;;; highlight-tail-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "highlight-tail" "highlight-tail.el" (22110
;;;;;;  64172 0 0))
;;; Generated autoloads from highlight-tail.el

(autoload 'highlight-tail-mode "highlight-tail" "\
Draw a \"tail\" while you're typing.

This minor-mode draws a tail in real time, when you write.  It
changes the background color of some last typed characters and
smoothly fade them out.

If ARG is 0 or less than zero then the mode will be disabled.
If ARG is nil then the mode will be switched.
If ARG is greater than zero then this mode will be turned on.

\(fn &optional ARG)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; highlight-tail-autoloads.el ends here
