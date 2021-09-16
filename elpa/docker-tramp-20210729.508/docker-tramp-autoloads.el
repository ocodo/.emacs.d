;;; docker-tramp-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "docker-tramp" "docker-tramp.el" (0 0 0 0))
;;; Generated autoloads from docker-tramp.el

(defvar docker-tramp-docker-options nil "\
List of docker options.")

(custom-autoload 'docker-tramp-docker-options "docker-tramp" t)

(defconst docker-tramp-completion-function-alist '((docker-tramp--parse-running-containers "")) "\
Default list of (FUNCTION FILE) pairs to be examined for docker method.")

(defconst docker-tramp-method "docker" "\
Method to connect docker containers.")

(autoload 'docker-tramp-cleanup "docker-tramp" "\
Cleanup TRAMP cache for docker method." t nil)

(autoload 'docker-tramp-add-method "docker-tramp" "\
Add docker tramp method." nil nil)

(eval-after-load 'tramp '(progn (docker-tramp-add-method) (tramp-set-completion-function docker-tramp-method docker-tramp-completion-function-alist)))

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "docker-tramp" '("docker-tramp-")))

;;;***

;;;### (autoloads nil nil ("docker-tramp-compat.el" "docker-tramp-pkg.el")
;;;;;;  (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; docker-tramp-autoloads.el ends here
