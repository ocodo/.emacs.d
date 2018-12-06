;;; synonymous-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "synonymous" "synonymous.el" (0 0 0 0))
;;; Generated autoloads from synonymous.el

(autoload 'synonymous-synonyms "synonymous" "\
Lookup synonyms for a word.

\(fn &optional EVENT OPOINT)" t nil)

(autoload 'synonymous-antonyms "synonymous" "\
Lookup antonyms for a word.

\(fn &optional EVENT OPOINT)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "synonymous" '("synonymous-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; synonymous-autoloads.el ends here
