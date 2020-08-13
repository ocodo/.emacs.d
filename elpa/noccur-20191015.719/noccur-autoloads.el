;;; noccur-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "noccur" "noccur.el" (0 0 0 0))
;;; Generated autoloads from noccur.el

(autoload 'noccur-dired "noccur" "\
Perform `multi-occur' with REGEXP in all dired marked files.
When called with a prefix argument NLINES, display NLINES lines before and after.

\(fn REGEXP &optional NLINES)" t nil)

(autoload 'noccur-project "noccur" "\
Perform `multi-occur' with REGEXP in the current project files.
When called with a prefix argument NLINES, display NLINES lines before and after.
If DIRECTORY-TO-SEARCH is specified, this directory will be searched recursively;
otherwise, the user will be prompted to specify a directory to search.

For performance reasons, files are filtered using 'find' or 'git
ls-files' and 'grep'.

\(fn REGEXP &optional NLINES DIRECTORY-TO-SEARCH)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "noccur" '("noccur--")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; noccur-autoloads.el ends here
