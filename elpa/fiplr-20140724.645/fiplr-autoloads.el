;;; fiplr-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "fiplr" "fiplr.el" (0 0 0 0))
;;; Generated autoloads from fiplr.el

(autoload 'fiplr-find-file "fiplr" "\
Runs a completing prompt to find a file from the project.
The root of the project is the return value of `fiplr-root'.

\(fn)" t nil)

(autoload 'fiplr-find-file-other-window "fiplr" "\
Runs a completing prompt to find a file from the project.
The root of the project is the return value of `fiplr-root'.  The
file is opened using `find-file-other-window'.

\(fn)" t nil)

(autoload 'fiplr-find-file-other-frame "fiplr" "\
Runs a completing prompt to find a file from the project.
The root of the project is the return value of `fiplr-root'.  The
file is opened using `find-file-other-frame'.

\(fn)" t nil)

(autoload 'fiplr-find-directory "fiplr" "\
Runs a completing prompt to find a directory from the project.
The root of the project is the return value of `fiplr-root'.

\(fn)" t nil)

(autoload 'fiplr-find-directory-other-window "fiplr" "\
Runs a completing prompt to find a directory from the project.
The root of the project is the return value of `fiplr-root'.  The
directory is opened using `dired-other-window'.

\(fn)" t nil)

(autoload 'fiplr-find-directory-other-frame "fiplr" "\
Runs a completing prompt to find a directory from the project.
The root of the project is the return value of `fiplr-root'.  The
directory is opened using `dired-other-frame'.

\(fn)" t nil)

(autoload 'fiplr-clear-cache "fiplr" "\
Clears the internal caches used by fiplr so the project is searched again.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "fiplr" '("fiplr-" "*fiplr-")))

;;;***

;;;### (autoloads nil nil ("fiplr-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; fiplr-autoloads.el ends here
