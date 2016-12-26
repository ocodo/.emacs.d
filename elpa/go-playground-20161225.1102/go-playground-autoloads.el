;;; go-playground-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "go-playground" "go-playground.el" (22624 31788
;;;;;;  56031 932000))
;;; Generated autoloads from go-playground.el

(autoload 'go-playground "go-playground" "\
Run playground for Go language in a new buffer.

\(fn)" t nil)

(autoload 'go-playground-remove-current-snippet "go-playground" "\
Obsoleted by `go-playground-rm'.

\(fn)" t nil)

(autoload 'go-playground-download "go-playground" "\
Download a paste from the play.golang.org and insert it in a new local playground buffer.
Tries to look for a URL at point.

\(fn URL)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; go-playground-autoloads.el ends here
