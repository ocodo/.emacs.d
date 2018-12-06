;;; heroku-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "heroku" "heroku.el" (0 0 0 0))
;;; Generated autoloads from heroku.el

(autoload 'heroku-sql "heroku" "\
Run heroku pg:psql as an inferior process in an SQL buffer.

Enter app name when prompted for `database'.

\(fn &optional BUFFER)" t nil)

(autoload 'heroku-run "heroku" "\
Run a remote command on a given app using `shell'.

\(fn &optional PROMPT-APP)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "heroku" '("heroku-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; heroku-autoloads.el ends here
