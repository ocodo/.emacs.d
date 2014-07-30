;;; paradox-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "paradox" "paradox.el" (21464 21513 438176
;;;;;;  999000))
;;; Generated autoloads from paradox.el

(autoload 'paradox--refresh-star-count "paradox" "\
Download the star-count file and populate the respective variable.

\(fn)" t nil)

(autoload 'paradox-list-packages "paradox" "\
Improved version of `package-list-packages'. The heart of paradox.
Function is equivalent to `package-list-packages' (including the
prefix NO-FETCH), but the resulting Package Menu is improved in
several ways.

Among them:

1. Uses `paradox-menu-mode', which has more functionality and
keybinds than `package-menu-mode'.

2. Uses some font-locking to improve readability.

3. Optionally shows the number GitHub stars and Melpa downloads
for packages.

4. Adds useful information in the mode-line.

\(fn NO-FETCH)" t nil)

(autoload 'paradox-require "paradox" "\
A replacement for `require' which also installs the feature if it is absent.
- If FEATURE is present, `require' it and return t.

- If FEATURE is not present, install PACKAGE with `package-install'.
If PACKAGE is nil, assume FEATURE is the package name.
After installation, `require' FEATURE.

FILENAME is passed to `require'.

If NOERROR is non-nil, don't complain if the feature couldn't be
installed, just return nil.

By default, the current package database (stored in
`package-archive-contents') is only updated if it is empty.
Passing a non-nil REFRESH argument forces this update.

\(fn FEATURE &optional FILENAME NOERROR PACKAGE REFRESH)" nil nil)

;;;***

;;;### (autoloads nil nil ("paradox-compat.el" "paradox-pkg.el")
;;;;;;  (21464 21513 445649 2000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; paradox-autoloads.el ends here
