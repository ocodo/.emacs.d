;;; dired-details+-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "dired-details+" "dired-details+.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from dired-details+.el

(defvar dired-details-hidden-string "" "\
*This string will be shown in place of file details and symbolic links.")

(custom-autoload 'dired-details-hidden-string "dired-details+" t)

(defvar dired-details-propagate-flag t "\
*Non-nil means next Dired buffer should be displayed the same.
The last `dired-details-state' value set is used by the next Dired
buffer created.")

(custom-autoload 'dired-details-propagate-flag "dired-details+" t)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "dired-details+" '("dired-details-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; dired-details+-autoloads.el ends here
