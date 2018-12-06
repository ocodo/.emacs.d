;;; vector-utils-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "vector-utils" "vector-utils.el" (0 0 0 0))
;;; Generated autoloads from vector-utils.el

(let ((loads (get 'vector-utils 'custom-loads))) (if (member '"vector-utils" loads) nil (put 'vector-utils 'custom-loads (cons '"vector-utils" loads))))

(autoload 'vector-utils-depth "vector-utils" "\
Find the depth of vector VEC, which may contain other vectors.

If VEC is not a vector or is an empty vector, returns a depth
of 0.

If VEC is vector which does not contain other vectors, returns
a depth of 1.

\(fn VEC)" nil nil)

(autoload 'vector-utils-flatten "vector-utils" "\
Flatten vector VEC, which may contain other vectors.

\(fn VEC)" nil nil)

(autoload 'vector-utils-insert-before-pos "vector-utils" "\
Look in VEC for position POS, and insert NEW-ELEMENT before.

POS is zero-indexed.

A modified copy of VEC is returned.

\(fn VEC POS NEW-ELEMENT)" nil nil)

(autoload 'vector-utils-insert-after-pos "vector-utils" "\
Look in VEC for position POS, and insert NEW-ELEMENT after.

POS is zero-indexed.

A modified copy of VEC is returned.

\(fn VEC POS NEW-ELEMENT)" nil nil)

(autoload 'vector-utils-insert-before "vector-utils" "\
Look in VEC for ELEMENT and insert NEW-ELEMENT before it.

A modified copy of VEC is returned.

\(fn VEC ELEMENT NEW-ELEMENT)" nil nil)

(autoload 'vector-utils-insert-after "vector-utils" "\
Look in VEC for ELEMENT and insert NEW-ELEMENT after it.

A modified copy of VEC is returned.

\(fn VEC ELEMENT NEW-ELEMENT)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "vector-utils" '("vector-utils--flatten-1")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; vector-utils-autoloads.el ends here
