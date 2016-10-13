;;; package-utils-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "package-utils" "package-utils.el" (22527 17422
;;;;;;  0 0))
;;; Generated autoloads from package-utils.el

(autoload 'package-utils-list-upgrades "package-utils" "\
List all packages that can be upgraded.

With prefix argument NO-FETCH, do not call `package-refresh-contents'.

\(fn &optional NO-FETCH)" t nil)

(autoload 'package-utils-upgrade-all "package-utils" "\
Upgrade all packages that can be upgraded.

With prefix argument NO-FETCH, do not call `package-refresh-contents'.
When DRY-RUN is true, only display what packages would be upgraded.

\(fn &optional NO-FETCH DRY-RUN)" t nil)

(autoload 'package-utils-upgrade-all-no-fetch "package-utils" "\
Upgrade all packages that can be upgraded without calling `package-refresh-contents' first.

\(fn)" t nil)

(autoload 'package-utils-upgrade-by-name "package-utils" "\
Upgrade the package NAME.

NAME can be a string or a symbol.

With prefix argument NO-FETCH, do not call `package-refresh-contents'.

\(fn NAME &optional NO-FETCH)" t nil)

(autoload 'package-utils-upgrade-by-name-no-fetch "package-utils" "\
Upgrade the package NAME, without calling `package-refresh-contents' first.

NAME can be a string or a symbol.

\(fn NAME)" t nil)

(autoload 'package-utils-remove-by-name "package-utils" "\
Uninstall the package NAME.

NAME can be a string or a symbol.

\(fn NAME)" t nil)

(autoload 'package-utils-list-packages-async "package-utils" "\
Like `package-list-packages', but works asynchronously.

\(fn)" t nil)

(autoload 'package-utils-install-async "package-utils" "\
Like `package-install', but works asynchronously.

\(fn PACKAGE)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; package-utils-autoloads.el ends here
