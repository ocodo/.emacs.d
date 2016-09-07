;;; 0blayout-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "0blayout" "0blayout.el" (22479 32553 757961
;;;;;;  328000))
;;; Generated autoloads from 0blayout.el

(autoload '0blayout-add-keybindings-with-prefix "0blayout" "\
Add 0blayout keybindings using the prefix PREFIX.

\(fn PREFIX)" nil nil)

(add-to-list 'default-frame-alist (cons '0blayout-current 0blayout-default))

(defvar 0blayout-mode nil "\
Non-nil if 0blayout mode is enabled.
See the `0blayout-mode' command
for a description of this minor mode.")

(custom-autoload '0blayout-mode "0blayout" nil)

(autoload '0blayout-mode "0blayout" "\
Handle layouts with ease

\(fn &optional ARG)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; 0blayout-autoloads.el ends here
