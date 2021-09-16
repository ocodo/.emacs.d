;;; dired-recent-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "dired-recent" "dired-recent.el" (0 0 0 0))
;;; Generated autoloads from dired-recent.el

(autoload 'dired-recent-open "dired-recent" "\
Show the dired history.  See: `dired-recent-mode'." t nil)

(defvar dired-recent-mode nil "\
Non-nil if Dired-Recent mode is enabled.
See the `dired-recent-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `dired-recent-mode'.")

(custom-autoload 'dired-recent-mode "dired-recent" nil)

(autoload 'dired-recent-mode "dired-recent" "\
Toggle `dired-recent-mode' on or off.
Turn `dired-recent-mode' if ARG is positive, off otherwise.
Turning it on makes dired save each opened path.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "dired-recent" '("dired-recent-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; dired-recent-autoloads.el ends here
