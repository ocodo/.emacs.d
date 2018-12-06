;;; ycmd-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "ycmd" "ycmd.el" (0 0 0 0))
;;; Generated autoloads from ycmd.el

(autoload 'ycmd-mode "ycmd" "\
Minor mode for interaction with the ycmd completion server.

When called interactively, toggle `ycmd-mode'.  With prefix ARG,
enable `ycmd-mode' if ARG is positive, otherwise disable it.

When called from Lisp, enable `ycmd-mode' if ARG is omitted,
nil or positive.  If ARG is `toggle', toggle `ycmd-mode'.
Otherwise behave as if called interactively.

\\{ycmd-mode-map}

\(fn &optional ARG)" t nil)

(autoload 'ycmd-setup "ycmd" "\
Setup `ycmd-mode'.

Hook `ycmd-mode' into modes in `ycmd-file-type-map'.

\(fn)" t nil)

(defvar global-ycmd-mode nil "\
Non-nil if Global Ycmd mode is enabled.
See the `global-ycmd-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-ycmd-mode'.")

(custom-autoload 'global-ycmd-mode "ycmd" nil)

(autoload 'global-ycmd-mode "ycmd" "\
Toggle Ycmd mode in all buffers.
With prefix ARG, enable Global Ycmd mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Ycmd mode is enabled in all buffers where
`ycmd--maybe-enable-mode' would do it.
See `ycmd-mode' for more information on Ycmd mode.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ycmd" '("ycmd-")))

;;;***

;;;### (autoloads nil "ycmd-eldoc" "ycmd-eldoc.el" (0 0 0 0))
;;; Generated autoloads from ycmd-eldoc.el

(autoload 'ycmd-eldoc-setup "ycmd-eldoc" "\
Setup eldoc for `ycmd-mode'.

\(fn)" t nil)

(autoload 'ycmd-eldoc-mode "ycmd-eldoc" "\
Toggle ycmd eldoc mode.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ycmd-eldoc" '("ycmd-eldoc-")))

;;;***

;;;### (autoloads nil "ycmd-next-error" "ycmd-next-error.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from ycmd-next-error.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ycmd-next-error" '("ycmd-next-error-")))

;;;***

;;;### (autoloads nil nil ("ycmd-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; ycmd-autoloads.el ends here
