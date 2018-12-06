;;; edit-server-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "edit-server" "edit-server.el" (0 0 0 0))
;;; Generated autoloads from edit-server.el

(autoload 'edit-server-start "edit-server" "\
Start the edit server.

If argument VERBOSE is non-nil, logs all server activity to buffer
`*edit-server-log*'.  When called interactivity, a prefix argument
will cause it to be verbose.

\(fn &optional VERBOSE)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "edit-server" '("edit-server-" "global-edit-server-edit-mode" "turn-on-edit-server-edit-mode-if-server")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; edit-server-autoloads.el ends here
