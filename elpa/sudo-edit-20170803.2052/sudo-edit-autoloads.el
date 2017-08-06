;;; sudo-edit-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "sudo-edit" "sudo-edit.el" (0 0 0 0))
;;; Generated autoloads from sudo-edit.el

(autoload 'sudo-edit "sudo-edit" "\
Edit currently visited file as another user, by default `sudo-edit-user'.

With a prefix ARG prompt for a file to visit.  Will also prompt
for a file to visit if current buffer is not visiting a file.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "sudo-edit" '("sudo-edit-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; sudo-edit-autoloads.el ends here
