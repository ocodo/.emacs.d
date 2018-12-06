;;; github-stars-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "github-stars" "github-stars.el" (0 0 0 0))
;;; Generated autoloads from github-stars.el

(autoload 'github-stars-browse-url "github-stars" "\
Prompt you for one of your github stars and open it in the web browser.

\(fn OWNER/NAME)" t nil)

(autoload 'github-stars-list "github-stars" "\
Display a list of your github stars.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "github-stars" '("github-stars")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; github-stars-autoloads.el ends here
