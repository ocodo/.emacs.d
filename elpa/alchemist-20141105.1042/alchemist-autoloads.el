;;; alchemist-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "alchemist" "alchemist.el" (21594 57010 313235
;;;;;;  56000))
;;; Generated autoloads from alchemist.el

(autoload 'alchemist-version "alchemist" "\
Get the Alchemist version as string.

If called interactively or if SHOW-VERSION is non-nil, show the
version in the echo area and the messages buffer.

The returned string includes both, the version from package.el
and the library version, if both a present and different.

If the version number could not be determined, signal an error,
if called interactively, or if SHOW-VERSION is non-nil, otherwise
just return nil.

\(fn &optional SHOW-VERSION)" t nil)

(autoload 'alchemist-mode "alchemist" "\
Toggle alchemist mode.

Key bindings:
\\{alchemist-mode-map}

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil nil ("alchemist-buffer.el" "alchemist-compile.el"
;;;;;;  "alchemist-execute.el" "alchemist-help.el" "alchemist-hooks.el"
;;;;;;  "alchemist-mix.el" "alchemist-pkg.el" "alchemist-project.el"
;;;;;;  "alchemist-utils.el") (21594 57010 383874 424000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; alchemist-autoloads.el ends here
