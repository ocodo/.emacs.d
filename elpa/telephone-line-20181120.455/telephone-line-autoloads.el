;;; telephone-line-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "telephone-line" "telephone-line.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from telephone-line.el

(defvar telephone-line-mode nil "\
Non-nil if Telephone-Line mode is enabled.
See the `telephone-line-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `telephone-line-mode'.")

(custom-autoload 'telephone-line-mode "telephone-line" nil)

(autoload 'telephone-line-mode "telephone-line" "\
Toggle telephone-line on or off.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "telephone-line" '("telephone-line-")))

;;;***

;;;### (autoloads nil "telephone-line-config" "telephone-line-config.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from telephone-line-config.el

(autoload 'telephone-line-evil-config "telephone-line-config" "\
Deprecated, just call (telephone-line-mode t) instead.

\(fn)" nil nil)

;;;***

;;;### (autoloads nil "telephone-line-segments" "telephone-line-segments.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from telephone-line-segments.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "telephone-line-segments" '("telephone-line-")))

;;;***

;;;### (autoloads nil "telephone-line-separators" "telephone-line-separators.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from telephone-line-separators.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "telephone-line-separators" '("telephone-line-")))

;;;***

;;;### (autoloads nil "telephone-line-utils" "telephone-line-utils.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from telephone-line-utils.el

(autoload 'telephone-line-defsegment* "telephone-line-utils" "\
Define NAME as a segment function.

Does not check if segment is empty; will always display on non-nil result.

\(fn NAME &rest BODY)" nil t)

(function-put 'telephone-line-defsegment* 'doc-string-elt '3)

(function-put 'telephone-line-defsegment* 'lisp-indent-function 'defun)

(autoload 'telephone-line-defsegment "telephone-line-utils" "\
Define NAME as a segment function.

Empty strings will not render.

\(fn NAME &rest BODY)" nil t)

(function-put 'telephone-line-defsegment 'doc-string-elt '3)

(function-put 'telephone-line-defsegment 'lisp-indent-function 'defun)

(autoload 'telephone-line-raw "telephone-line-utils" "\
Conditionally render STR as mode-line data, or just verify output if not PREFORMATTED.
Return nil for blank/empty strings.

\(fn STR &optional PREFORMATTED)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "telephone-line-utils" '("telephone-line-")))

;;;***

;;;### (autoloads nil nil ("telephone-line-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; telephone-line-autoloads.el ends here
