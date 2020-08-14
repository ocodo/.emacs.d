;;; loccur-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "loccur" "loccur.el" (0 0 0 0))
;;; Generated autoloads from loccur.el

(autoload 'loccur-current "loccur" "\
Call `loccur' for the current word." t nil)

(autoload 'loccur "loccur" "\
Perform a simple grep in current buffer.

This command hides all lines from the current buffer except those
containing the regular expression REGEX.  A second call of the function
unhides lines again.

When called interactively, either prompts the user for REGEXP or,
when called with an active region, uses the content of the
region, unless called with the universal prefix (C-u)

\(fn REGEX)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "loccur" '("loccur-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; loccur-autoloads.el ends here
