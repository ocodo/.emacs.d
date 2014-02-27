;;; flymake-jslint-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (flymake-jslint-load flymake-jslint-args flymake-jslint-command
;;;;;;  flymake-jslint-detect-trailing-comma) "flymake-jslint" "flymake-jslint.el"
;;;;;;  (21262 52299 0 0))
;;; Generated autoloads from flymake-jslint.el

(defvar flymake-jslint-detect-trailing-comma t "\
Whether or not to report warnings about trailing commas.")

(custom-autoload 'flymake-jslint-detect-trailing-comma "flymake-jslint" t)

(defvar flymake-jslint-command (if (executable-find "jsl") "jsl" "jslint") "\
Name (and optionally full path) of jslint executable.")

(custom-autoload 'flymake-jslint-command "flymake-jslint" t)

(defvar flymake-jslint-args (unless (string-equal "jsl" flymake-jslint-command) (mapcar 'symbol-name '(--white --undef --nomen --regexp --plusplus --bitwise --newcap --sloppy --vars --eqeq))) "\
Command-line args for jslint executable.")

(custom-autoload 'flymake-jslint-args "flymake-jslint" t)

(autoload 'flymake-jslint-load "flymake-jslint" "\
Configure flymake mode to check the current buffer's javascript syntax.

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("flymake-jslint-pkg.el") (21262 52299
;;;;;;  293822 0))

;;;***

(provide 'flymake-jslint-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; flymake-jslint-autoloads.el ends here
