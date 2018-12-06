;;; sort-words-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "sort-words" "sort-words.el" (0 0 0 0))
;;; Generated autoloads from sort-words.el

(autoload 'sort-words "sort-words" "\
Sort words in region alphabetically.
Then insert them replacing the existing region.
START and END are boundries of the selected region.

\(fn START END)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "sort-words" '("sort-words-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; sort-words-autoloads.el ends here
