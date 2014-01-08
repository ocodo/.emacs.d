;;; pivotal-tracker-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (pivotal-get-projects pivotal) "pivotal-tracker"
;;;;;;  "pivotal-tracker.el" (21196 38618 0 0))
;;; Generated autoloads from pivotal-tracker.el

(defgroup pivotal nil "\
Pivotal Tracker" :group (quote external))

(defcustom pivotal-api-token "" "\
API key found on the /profile page of pivotal tracker" :group (quote pivotal) :type (quote string))

(autoload 'pivotal "pivotal-tracker" "\
launch pivotal-projects window, or just switch to it

\(fn)" t nil)

(autoload 'pivotal-get-projects "pivotal-tracker" "\
show a buffer of all projects you have access to

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("pivotal-tracker-pkg.el") (21196 38618
;;;;;;  744525 0))

;;;***

(provide 'pivotal-tracker-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; pivotal-tracker-autoloads.el ends here
