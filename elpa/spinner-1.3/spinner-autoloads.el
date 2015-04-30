;;; spinner-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "spinner" "spinner.el" (21826 33755 827401
;;;;;;  0))
;;; Generated autoloads from spinner.el

(defstruct (spinner (:copier nil) (:conc-name spinner--) (:constructor make-spinner (&optional type buffer-local fps) "Create a spinner of the given TYPE.\nThe possible TYPEs are described in `spinner--type-to-frames'.\n\nFPS, if given, is the number of desired frames per second.\nDefault is `spinner-frames-per-second'.\n\nIf BUFFER-LOCAL is non-nil, the spinner will be automatically\ndeactivated if the buffer is killed.  If BUFFER-LOCAL is a\nbuffer, use that instead of current buffer.\n\nWhen started, in order to function properly, the spinner runs a\ntimer which periodically calls `force-mode-line-update' in the\ncurent buffer.  If BUFFER-LOCAL was set at creation time, then\n`force-mode-line-update' is called in that buffer instead.  When\nthe spinner is stopped, the timer is deactivated.")) (frames (spinner--type-to-frames type)) (counter 0) (fps spinner-frames-per-second) (timer (timer-create) :read-only) (active-p nil) (buffer (when buffer-local (if (bufferp buffer-local) buffer-local (current-buffer)))))

(autoload 'spinner-start "spinner" "\
Start a mode-line spinner of given TYPE-OR-OBJECT.
If TYPE-OR-OBJECT is an object created with `make-spinner',
simply activate it.  This method is designed for minor modes, so
they can use the spinner as part of their lighter by doing:
    '(:eval (spinner-print THE-SPINNER))
To stop this spinner, call `spinner-stop' on it.

If TYPE-OR-OBJECT is anything else, a buffer-local spinner is
created with this type, and it is displayed in the
`mode-line-process' of the buffer it was created it.  Both
TYPE-OR-OBJECT and FPS are passed to `make-spinner' (which see).
To stop this spinner, call `spinner-stop' in the same buffer.

Either way, the return value is a function which can be called
anywhere to stop this spinner.  You can also call `spinner-stop'
in the same buffer where the spinner was created.

FPS, if given, is the number of desired frames per second.
Default is `spinner-frames-per-second'.

\(fn &optional TYPE-OR-OBJECT FPS)" nil nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; spinner-autoloads.el ends here
