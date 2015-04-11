;;; spinner-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "spinner" "spinner.el" (21800 59880 363401
;;;;;;  0))
;;; Generated autoloads from spinner.el

(autoload 'spinner-start "spinner" "\
Start a mode-line spinner of given TYPE.
Spinners are buffer local. It is added to the mode-line in the
buffer where `spinner-start' is called.

Return value is a function which can be called anywhere to stop
this spinner.  You can also call `spinner-stop' in the same
buffer where the spinner was created.

FPS, if given, is the number of desired frames per second.
Default is `spinner-frames-per-second'.

If NOADD is non-nil, the spinner is not added to the mode-line.
It is then your responsibility to add the symbol
`spinner--mode-line-construct' somewhere in the mode-line,
probably as part of a minor-mode lighter.

If TYPE is nil, use the first element of `spinner-types'.
If TYPE is `random', use a random element of `spinner-types'.
If it is a symbol, it specifies an element of `spinner-types'.
If it is a vector, it used as the spinner.
If it is a list, it should be a list of symbols, and a random one
is chosen as the spinner type.

\(fn &optional TYPE FPS NOADD)" nil nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; spinner-autoloads.el ends here
