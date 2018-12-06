;;; haskell-emacs-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "haskell-emacs" "haskell-emacs.el" (0 0 0 0))
;;; Generated autoloads from haskell-emacs.el

(autoload 'haskell-emacs-help "haskell-emacs" "\
Display the documentation for haskell-emacs.

\(fn)" t nil)

(autoload 'haskell-emacs-init "haskell-emacs" "\
Initialize haskell FFI or reload it to reflect changed functions.

When ARG, force installation dialog.
Call `haskell-emacs-help' to read the documentation.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "haskell-emacs" '("haskell-emacs-")))

;;;***

;;;### (autoloads nil nil ("haskell-emacs-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; haskell-emacs-autoloads.el ends here
