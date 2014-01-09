;;; multi-web-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (multi-web-global-mode multi-web-mode) "multi-web-mode"
;;;;;;  "multi-web-mode.el" (21017 58994 0 0))
;;; Generated autoloads from multi-web-mode.el

(autoload 'multi-web-mode "multi-web-mode" "\
Enables the multi web mode chunk detection and indentation

\(fn &optional ARG)" t nil)

(defvar multi-web-global-mode nil "\
Non-nil if Multi-Web-Global mode is enabled.
See the command `multi-web-global-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `multi-web-global-mode'.")

(custom-autoload 'multi-web-global-mode "multi-web-mode" nil)

(autoload 'multi-web-global-mode "multi-web-mode" "\
Toggle Multi-Web mode in all buffers.
With prefix ARG, enable Multi-Web-Global mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Multi-Web mode is enabled in all buffers where
`multi-web-mode-maybe' would do it.
See `multi-web-mode' for more information on Multi-Web mode.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil nil ("multi-web-mode-pkg.el" "mweb-example-config.el")
;;;;;;  (21017 58994 117888 0))

;;;***

(provide 'multi-web-mode-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; multi-web-mode-autoloads.el ends here
