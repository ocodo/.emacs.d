;;; dired-efap-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "dired-efap" "dired-efap.el" (21440 45242 774176
;;;;;;  999000))
;;; Generated autoloads from dired-efap.el

(autoload 'dired-efap-click "dired-efap" "\
Move to the point and, if needed, edit filename at point.

Depending of the value of `dired-efap-use-mouse', if EVENT is a
doubleclick, and the previous position of the point, edit
filename at point.

See `dired-efap-use-mouse' and `dired-efap'

\(fn EVENT)" t nil)

(autoload 'dired-efap "dired-efap" "\
Make the filename at point editable by user.
Press RET to actually rename the file or directory in disk, and
C-g to abort.

If FROM-MOUSE is not nil, the mode is being set because of a mouse event.

\(fn &optional FROM-MOUSE)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; dired-efap-autoloads.el ends here
