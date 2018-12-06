;;; skewer-reload-stylesheets-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "skewer-reload-stylesheets" "skewer-reload-stylesheets.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from skewer-reload-stylesheets.el

(autoload 'skewer-reload-stylesheets-start-editing "skewer-reload-stylesheets" "\
Configure current buffer for live-editing.

Add this to your stylesheet editing mode hook to make every
stylesheet live-editable by default.

\(fn)" nil nil)

(autoload 'skewer-reload-stylesheets-mode "skewer-reload-stylesheets" "\
Minor mode for interactively reloading CSS stylesheets.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "skewer-reload-stylesheets" '("skewer-reload-stylesheets-")))

;;;***

;;;### (autoloads nil nil ("skewer-reload-stylesheets-pkg.el") (0
;;;;;;  0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; skewer-reload-stylesheets-autoloads.el ends here
