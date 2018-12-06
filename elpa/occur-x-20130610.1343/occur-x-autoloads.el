;;; occur-x-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "occur-x" "occur-x.el" (0 0 0 0))
;;; Generated autoloads from occur-x.el

(autoload 'occur-x-mode "occur-x" "\
Add some extra functionality to occur-mode.

User can refine the occur matches with any number of extra regexp
based filters.

Also, the line numbers are displayed in the margin of your
choice, instead of inside the occur buffer.  This way every match
line in the occur buffer is exactly the same as in the original
buffer.  See variable `occur-linenumbers-in-margin' and face
`occur-margin-face'.  When displayed in the margin, line numbers
won't interfere with the regexps of the additional filters.

\\{occur-x-mode-map}

\(fn &optional ARG)" t nil)

(autoload 'turn-on-occur-x-mode "occur-x" "\
Turn on occur-x-mode unconditionally.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "occur-x" '("occur-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; occur-x-autoloads.el ends here
