;;; wrap-region-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (wrap-region-global-mode turn-off-wrap-region-mode
;;;;;;  turn-on-wrap-region-mode wrap-region-mode) "wrap-region"
;;;;;;  "wrap-region.el" (20960 50523 0 0))
;;; Generated autoloads from wrap-region.el

(autoload 'wrap-region-mode "wrap-region" "\
Wrap region with stuff.

\(fn &optional ARG)" t nil)

(autoload 'turn-on-wrap-region-mode "wrap-region" "\
Turn on `wrap-region-mode'.

\(fn)" t nil)

(autoload 'turn-off-wrap-region-mode "wrap-region" "\
Turn off `wrap-region-mode'.

\(fn)" t nil)

(defvar wrap-region-global-mode nil "\
Non-nil if Wrap-Region-Global mode is enabled.
See the command `wrap-region-global-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `wrap-region-global-mode'.")

(custom-autoload 'wrap-region-global-mode "wrap-region" nil)

(autoload 'wrap-region-global-mode "wrap-region" "\
Toggle Wrap-Region mode in all buffers.
With prefix ARG, enable Wrap-Region-Global mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Wrap-Region mode is enabled in all buffers where
`turn-on-wrap-region-mode' would do it.
See `wrap-region-mode' for more information on Wrap-Region mode.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil nil ("wrap-region-pkg.el") (20960 50523 602213
;;;;;;  0))

;;;***

(provide 'wrap-region-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; wrap-region-autoloads.el ends here
