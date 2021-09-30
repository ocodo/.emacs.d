;;; butler-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "butler" "butler.el" (0 0 0 0))
;;; Generated autoloads from butler.el

(autoload 'butler-status "butler" "\
Shows the butler status buffer which displays the status of the configured CI server" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "butler" '("butler-" "draw-" "find-current-" "generate-progress-string" "hide-butler-job" "parse-jobs" "refresh-butler-status" "trigger-butler-job")))

;;;***

;;;### (autoloads nil "butler-servers" "butler-servers.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from butler-servers.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "butler-servers" '("butler-" "generate-basic-auth" "get-" "parse-authinfo-file" "prepare-servers")))

;;;***

;;;### (autoloads nil "butler-util" "butler-util.el" (0 0 0 0))
;;; Generated autoloads from butler-util.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "butler-util" '("colorize-dot")))

;;;***

;;;### (autoloads nil nil ("butler-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; butler-autoloads.el ends here
