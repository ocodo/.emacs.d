;;; ido-load-library-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "ido-load-library" "ido-load-library.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from ido-load-library.el

(let ((loads (get 'ido-load-library 'custom-loads))) (if (member '"ido-load-library" loads) nil (put 'ido-load-library 'custom-loads (cons '"ido-load-library" loads))))

(autoload 'ido-load-library "ido-load-library" "\
Load the Emacs Lisp library named LIBRARY.

This is identical to `load-library' except that is uses
`ido-completing-read' and a specialized history.

To set REGENERATE and reload the cache of library names, use a
universal prefix argument.

\(fn LIBRARY &optional REGENERATE)" t nil)

(autoload 'ido-load-library-find "ido-load-library" "\
Open the Emacs Lisp library named FILE for editing.

Uses `ido-completing-read' to find any library on `load-path' for
visiting in a buffer.  Text around the point will be used for the
default value, making this something of an alternative to
`find-file-at-point'.

To set REGENERATE and reload the cache of library names, use a
universal prefix argument.

\(fn FILE &optional REGENERATE)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ido-load-library" '("ido-load-library-" "persistent-softest-" "library-name-history")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; ido-load-library-autoloads.el ends here
