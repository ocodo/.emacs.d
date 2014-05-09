;;; package+-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (package-manifest) "package+" "package+.el" (21356
;;;;;;  51522 0 0))
;;; Generated autoloads from package+.el

(autoload 'package-manifest "package+" "\
Ensures MANIFEST is installed and uninstalls other packages.
MANIFEST declares a list of packages that should be installed on
this system, installing any missing packages and removing any
installed packages that are not in the manifest.

This makes it easy to keep a list of packages under version
control and replicated across all your environments, without
having to have all the packages themselves under version
control.

\(fn &rest MANIFEST)" nil nil)

;;;***

;;;### (autoloads nil nil ("package+-pkg.el") (21356 51522 858277
;;;;;;  0))

;;;***

(provide 'package+-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; package+-autoloads.el ends here
