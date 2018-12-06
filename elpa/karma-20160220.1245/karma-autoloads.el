;;; karma-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "karma" "karma.el" (0 0 0 0))
;;; Generated autoloads from karma.el

(autoload 'karma-version "karma" "\
Get the Karma version as string.

If called interactively or if SHOW-VERSION is non-nil, show the
version in the echo area and the messages buffer.

The returned string includes both, the version from package.el
and the library version, if both a present and different.

If the version number could not be determined, signal an error,
if called interactively, or if SHOW-VERSION is non-nil, otherwise
just return nil.

\(fn &optional SHOW-VERSION)" t nil)

(autoload 'karma-mode "karma" "\
Toggle karma mode.

Key bindings:
\\{karma-mode-map}

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "karma" '("karma-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; karma-autoloads.el ends here
