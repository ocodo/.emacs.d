;;; pivotal-tracker-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "pivotal-tracker" "pivotal-tracker.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from pivotal-tracker.el

(defgroup pivotal nil "\
Pivotal Tracker" :group (quote external))

(defcustom pivotal-api-token "" "\
API key found on the /profile page of pivotal tracker" :group (quote pivotal) :type (quote string))

(autoload 'pivotal "pivotal-tracker" "\
Launch pivotal-projects window, or just switch to it.

\(fn)" t nil)

(autoload 'pivotal-get-projects "pivotal-tracker" "\
Show a buffer of all projects you have access to.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pivotal-tracker" '("pivotal-" "assert-pivotal-api-token" "*pivotal-iteration*")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; pivotal-tracker-autoloads.el ends here
