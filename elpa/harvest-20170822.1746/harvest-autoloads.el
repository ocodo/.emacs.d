;;; harvest-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "harvest" "harvest.el" (0 0 0 0))
;;; Generated autoloads from harvest.el

(autoload 'harvest "harvest" "\
Start the main Harvest hydra.

\(fn)" t nil)

(autoload 'harvest-clock-out "harvest" "\
Clock out of any active timer.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "harvest" '("hydra-harvest" "harvest-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; harvest-autoloads.el ends here
