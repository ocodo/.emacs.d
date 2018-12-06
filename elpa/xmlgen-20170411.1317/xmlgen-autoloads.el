;;; xmlgen-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "xmlgen" "xmlgen.el" (0 0 0 0))
;;; Generated autoloads from xmlgen.el

(autoload 'xmlgen "xmlgen" "\
Convert a sexp FORM to xml:
\\='(p :class \"big\")) => \"<p class=\\\"big\\\" />\".
IN-ELM is ignored.  LEVEL is the element level and defaults to 0.

\(fn FORM &optional IN-ELM LEVEL)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "xmlgen" '("xmlgen-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; xmlgen-autoloads.el ends here
