;;; pkg-info-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (pkg-info-package-version pkg-info-defining-library-version
;;;;;;  pkg-info-library-version) "pkg-info" "pkg-info.el" (21097
;;;;;;  5857 0 0))
;;; Generated autoloads from pkg-info.el

(autoload 'pkg-info-library-version "pkg-info" "\
Get the version in the header of LIBRARY.

LIBRARY is either a symbol denoting a named feature, or a library
name as string..

When SHOW is non-nil, show the version in the minibuffer.

Return the version from the header of LIBRARY as list.  Signal an
error if the LIBRARY was not found or had no proper header.

See Info node `(elisp)Library Headers' for more information
about library headers.

\(fn LIBRARY &optional SHOW)" t nil)

(autoload 'pkg-info-defining-library-version "pkg-info" "\
Get the version of the library defining FUNCTION.

When SHOW is non-nil, show the version in mini-buffer.

This function is mainly intended to find the version of a major
or minor mode, i.e.

   (pkg-info-defining-library-version 'flycheck-mode)

Return the version of the library defining FUNCTION (as by
`pkg-info-locate-library-version').  Signal an error if FUNCTION
is not a valid function, if its defining library was not found,
or if the library had no proper version header.

\(fn FUNCTION &optional SHOW)" t nil)

(autoload 'pkg-info-package-version "pkg-info" "\
Get the version of an installed PACKAGE.

When SHOW is non-nil, show the version in the minibuffer.

Return the version as list, or nil if PACKAGE is not installed.

\(fn PACKAGE &optional SHOW)" t nil)

;;;***

;;;### (autoloads nil nil ("pkg-info-pkg.el") (21097 5857 855567
;;;;;;  0))

;;;***

(provide 'pkg-info-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; pkg-info-autoloads.el ends here
