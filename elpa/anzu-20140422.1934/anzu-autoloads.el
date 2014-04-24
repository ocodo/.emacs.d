;;; anzu-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (anzu-replace-at-cursor-thing anzu-query-replace-regexp
;;;;;;  anzu-query-replace anzu-query-replace-at-cursor-thing anzu-query-replace-at-cursor
;;;;;;  global-anzu-mode anzu-mode) "anzu" "anzu.el" (21337 1930
;;;;;;  0 0))
;;; Generated autoloads from anzu.el

(autoload 'anzu-mode "anzu" "\
minor-mode which display search information in mode-line.

\(fn &optional ARG)" t nil)

(defvar global-anzu-mode nil "\
Non-nil if Global-Anzu mode is enabled.
See the command `global-anzu-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-anzu-mode'.")

(custom-autoload 'global-anzu-mode "anzu" nil)

(autoload 'global-anzu-mode "anzu" "\
Toggle Anzu mode in all buffers.
With prefix ARG, enable Global-Anzu mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Anzu mode is enabled in all buffers where
`anzu--turn-on' would do it.
See `anzu-mode' for more information on Anzu mode.

\(fn &optional ARG)" t nil)

(autoload 'anzu-query-replace-at-cursor "anzu" "\


\(fn)" t nil)

(autoload 'anzu-query-replace-at-cursor-thing "anzu" "\


\(fn)" t nil)

(autoload 'anzu-query-replace "anzu" "\


\(fn ARG)" t nil)

(autoload 'anzu-query-replace-regexp "anzu" "\


\(fn ARG)" t nil)

(autoload 'anzu-replace-at-cursor-thing "anzu" "\


\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("anzu-pkg.el") (21337 1930 96201 0))

;;;***

(provide 'anzu-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; anzu-autoloads.el ends here
