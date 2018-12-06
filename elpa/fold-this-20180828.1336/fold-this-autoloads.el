;;; fold-this-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "fold-this" "fold-this.el" (0 0 0 0))
;;; Generated autoloads from fold-this.el

(autoload 'fold-this "fold-this" "\
Fold the region between BEG and END.

If FOLD-HEADER is specified, show this text in place of the
folded region.  If not, default to three dots: ...

\(fn BEG END &optional FOLD-HEADER)" t nil)

(autoload 'fold-this-sexp "fold-this" "\
Fold sexp around point.

If the point is at a symbol, fold the parent sexp.  If the point
is in front of a sexp, fold the following sexp.

\(fn)" t nil)

(autoload 'fold-this-all "fold-this" "\


\(fn BEG END)" t nil)

(autoload 'fold-active-region "fold-this" "\


\(fn BEG END)" t nil)

(autoload 'fold-active-region-all "fold-this" "\


\(fn BEG END)" t nil)

(autoload 'fold-this-unfold-all "fold-this" "\


\(fn)" t nil)

(autoload 'fold-this-unfold-at-point "fold-this" "\


\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "fold-this" '("fold-this-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; fold-this-autoloads.el ends here
