;;; dim-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "dim" "dim.el" (0 0 0 0))
;;; Generated autoloads from dim.el

(autoload 'dim-major-name "dim" "\
Set mode-line name of the major MODE to NEW-NAME.
The change will take effect next time the MODE will be enabled.

\(fn MODE NEW-NAME)" nil nil)

(autoload 'dim-major-names "dim" "\
Change names of major modes according to SPECS list.
Each element of the list should be a list of arguments taken by
`dim-major-name' function.

\(fn SPECS)" nil nil)

(autoload 'dim-minor-name "dim" "\
Set mode-line name of the minor MODE to NEW-NAME.
FILE is a feature or file name where the MODE comes from.  If it
is specified, it is passed to `eval-after-load'.  If it is nil,
MODE name is changed immediately (if the MODE is available).

\(fn MODE NEW-NAME &optional FILE)" nil nil)

(autoload 'dim-minor-names "dim" "\
Change names of minor modes according to SPECS list.
Each element of the list should be a list of arguments taken by
`dim-minor-name' function.

\(fn SPECS)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "dim" '("dim-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; dim-autoloads.el ends here
