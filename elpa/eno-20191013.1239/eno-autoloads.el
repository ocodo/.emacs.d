;;; eno-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "eno" "eno.el" (0 0 0 0))
;;; Generated autoloads from eno.el

(autoload 'eno "eno" "\
show matching regexp with hints then return the beginning and end of the selected hint(overlay).

\(fn RE &optional AT-HEAD ASIDE)" nil nil)

(autoload 'eno-word-goto "eno" nil t nil)

(autoload 'eno-word-cut "eno" nil t nil)

(autoload 'eno-word-copy "eno" nil t nil)

(autoload 'eno-word-paste "eno" nil t nil)

(autoload 'eno-symbol-goto "eno" nil t nil)

(autoload 'eno-symbol-cut "eno" nil t nil)

(autoload 'eno-symbol-copy "eno" nil t nil)

(autoload 'eno-symbol-paste "eno" nil t nil)

(autoload 'eno-str-goto "eno" nil t nil)

(autoload 'eno-str-cut "eno" nil t nil)

(autoload 'eno-str-copy "eno" nil t nil)

(autoload 'eno-str-paste "eno" nil t nil)

(autoload 'eno-line-goto "eno" nil t nil)

(autoload 'eno-line-cut "eno" nil t nil)

(autoload 'eno-line-copy "eno" nil t nil)

(autoload 'eno-line-paste "eno" nil t nil)

(autoload 'eno-line-comment "eno" nil t nil)

(autoload 'eno-line-return "eno" "\
simulate return at line end" t nil)

(autoload 'eno-paren-goto "eno" nil t nil)

(autoload 'eno-paren-copy "eno" nil t nil)

(autoload 'eno-paren-cut "eno" nil t nil)

(autoload 'eno-paren-delete "eno" nil t nil)

(autoload 'eno-paren-paste "eno" nil t nil)

(autoload 'eno-line-comment-to "eno" nil t nil)

(autoload 'eno-line-copy-to "eno" nil t nil)

(autoload 'eno-line-cut-to "eno" nil t nil)

(autoload 'eno-line-delete-to "eno" nil t nil)

(autoload 'eno-line-paste-to "eno" nil t nil)

(autoload 'eno-symbol-copy-to "eno" nil t nil)

(autoload 'eno-symbol-cut-to "eno" nil t nil)

(autoload 'eno-symbol-delete-to "eno" nil t nil)

(autoload 'eno-symbol-paste-to "eno" nil t nil)

(autoload 'eno-word-goto-inline "eno" nil t nil)

(autoload 'eno-word-copy-to-inline "eno" nil t nil)

(autoload 'eno-word-cut-to-inline "eno" nil t nil)

(autoload 'eno-word-paste-to-inline "eno" nil t nil)

(autoload 'eno-line-comment-from-to "eno" nil t nil)

(autoload 'eno-line-copy-from-to "eno" nil t nil)

(autoload 'eno-line-cut-from-to "eno" nil t nil)

(autoload 'eno-line-delete-from-to "eno" nil t nil)

(autoload 'eno-line-paste-from-to "eno" nil t nil)

(autoload 'eno-symbol-copy-from-to "eno" nil t nil)

(autoload 'eno-symbol-cut-from-to "eno" nil t nil)

(autoload 'eno-symbol-delete-from-to "eno" nil t nil)

(autoload 'eno-symbol-paste-from-to "eno" nil t nil)

(autoload 'eno-word-swap "eno" nil t nil)

(autoload 'eno-symbol-swap "eno" nil t nil)

(autoload 'eno-url-open "eno" nil t nil)

(autoload 'eno-clear-overlay "eno" nil t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "eno" '("eno-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; eno-autoloads.el ends here
