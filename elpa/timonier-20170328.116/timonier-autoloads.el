;;; timonier-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "timonier-custom" "timonier-custom.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from timonier-custom.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "timonier-custom" '("timonier-")))

;;;***

;;;### (autoloads nil "timonier-io" "timonier-io.el" (0 0 0 0))
;;; Generated autoloads from timonier-io.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "timonier-io" '("timonier--assoc-cdr")))

;;;***

;;;### (autoloads nil "timonier-k8s" "timonier-k8s.el" (0 0 0 0))
;;; Generated autoloads from timonier-k8s.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "timonier-k8s" '("timonier--")))

;;;***

;;;### (autoloads nil "timonier-mode" "timonier-mode.el" (0 0 0 0))
;;; Generated autoloads from timonier-mode.el

(autoload 'timonier-k8s "timonier-mode" "\
Display informations about the Kubernetes cluster.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "timonier-mode" '("timonier-")))

;;;***

;;;### (autoloads nil "timonier-utils" "timonier-utils.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from timonier-utils.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "timonier-utils" '("timonier-")))

;;;***

;;;### (autoloads nil "timonier-version" "timonier-version.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from timonier-version.el

(autoload 'timonier-version "timonier-version" "\
Get the timonier version as string.
If called interactively or if SHOW-VERSION is non-nil, show the
version in the echo area and the messages buffer.
The returned string includes both, the version from package.el
and the library version, if both a present and different.
If the version number could not be determined, signal an error,
if called interactively, or if SHOW-VERSION is non-nil, otherwise
just return nil.

\(fn &optional SHOW-VERSION)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "timonier-version" '("timonier--library-version")))

;;;***

;;;### (autoloads nil nil ("timonier-pkg.el" "timonier.el") (0 0
;;;;;;  0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; timonier-autoloads.el ends here
