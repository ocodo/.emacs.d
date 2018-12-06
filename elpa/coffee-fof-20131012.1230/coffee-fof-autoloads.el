;;; coffee-fof-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "coffee-fof" "coffee-fof.el" (0 0 0 0))
;;; Generated autoloads from coffee-fof.el

(autoload 'coffee-find-other-file "coffee-fof" "\
Find the CoffeeScript or JavaScript file corresponding to this file.

If optional IN-OTHER-WINDOW is non-nil, find the file in the other window.
For more Information, See `ff-find-other-file' function.

\(fn &optional IN-OTHER-WINDOW)" t nil)

(autoload 'coffee-find-test-file "coffee-fof" "\
Find the CoffeeScrpt/JavaScript or test file corresponding to this file.

If optional IN-OTHER-WINDOW is non-nil, find the file in the other window.
For more Information, See `ff-find-other-file' function.

\(fn &optional IN-OTHER-WINDOW)" t nil)

(autoload 'coffee-fof-setup "coffee-fof" "\
Setup coffee-fof.

Keywords supported:
:other-key :test-key

PLIST is a list like (:key1 val1 :key2 val2 ...).

Basic keywords are the following:

:other-key

Give key a local binding as `coffee-find-other-file'
in `js-mode-map', `js2-mode-map', `js3-mode-map' and `coffee-mode-map'.

:test-key

Give key a local binding as `coffee-find-test-file'
in `js-mode-map', `js2-mode-map', `js3-mode-map' and `coffee-mode-map'.

\(fn &rest PLIST)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "coffee-fof" '("coffee-f")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; coffee-fof-autoloads.el ends here
