;;; ido-occur-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "ido-occur" "ido-occur.el" (0 0 0 0))
;;; Generated autoloads from ido-occur.el

(autoload 'ido-occur "ido-occur" "\
Yet another `occur' with `ido'.
When non-nil, QUERY is the initial search pattern.

\(fn &optional QUERY)" t nil)

(autoload 'ido-occur-at-point "ido-occur" "\
Open `ido-occur' at point.

\(fn)" t nil)

(autoload 'ido-occur-from-isearch "ido-occur" "\
Open `ido-occur' from `isearch'.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ido-occur" '("ido-occur--")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; ido-occur-autoloads.el ends here
