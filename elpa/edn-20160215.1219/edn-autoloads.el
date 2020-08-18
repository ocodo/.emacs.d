;;; edn-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "edn" "edn.el" (0 0 0 0))
;;; Generated autoloads from edn.el

(autoload 'edn-time-to-inst "edn" "\
Turn a `time-date' TIME into our internal representation of an inst.

\(fn TIME)" nil nil)

(autoload 'edn-inst-to-time "edn" "\
Turn an `edn-inst', INST, into a TIME from `time-date'.

\(fn INST)" nil nil)

(autoload 'edn-string-to-uuid "edn" "\
Create an `edn-uuid' from a string, S, containing a uuid.

\(fn S)" nil nil)

(autoload 'edn-uuid-to-string "edn" "\
Turn our internal representation of a UUID into a string.

\(fn UUID)" nil nil)

(autoload 'edn-read "edn" "\
Read one edn value from SOURCE.

SOURCE is either a string of edn data or nil.  If no source is
given the next edn value will be read from POINT in the current
buffer.

You can use `edn-add-reader' to add your own readers for unknown
tags.

\(fn &optional SOURCE)" nil nil)

(autoload 'edn-list-to-set "edn" "\
Turn a list into `edn''s internal set representation.

If COMPARE-FN is provided this function is used to uniquify the
list.  Otherwise it's expected that l is without duplicates.

\(fn L &optional COMPARE-FN)" nil nil)

(autoload 'edn-set-to-list "edn" "\
Turn `edn''s internal set representation into a list.

\(fn S)" nil nil)

(autoload 'edn-add-reader "edn" "\
Add a READER function for TAG.

TAG is either a string, symbol or keyword. e.g. :my/type

\(fn TAG READER)" nil nil)

(autoload 'edn-add-writer "edn" "\
Add a WRITER function for types satisfying PRED.

\(fn PRED WRITER)" nil nil)

(autoload 'edn-remove-reader "edn" "\
Remove a previously registered handler for TAG.

\(fn TAG)" nil nil)

(autoload 'edn-remove-writer "edn" "\
The remove the writer WRITER.

\(fn WRITER)" nil nil)

(autoload 'edn-print-string "edn" "\
Serialize the lisp form DATUM into edn.

You can use `edn-add-writer' to add writers capable of writing
your own tagged data.

\(fn DATUM)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "edn" '("edn--" "hash-table-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; edn-autoloads.el ends here
