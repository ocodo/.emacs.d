;;; groovy-imports-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "groovy-imports" "groovy-imports.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from groovy-imports.el

(autoload 'groovy-imports-scan-file "groovy-imports" "\
Scan a groovy-mode buffer, adding any import class -> package
mappings to the import cache. If called with a prefix arguments
overwrites any existing cache entries for the file.

\(fn)" t nil)

(autoload 'groovy-imports-list-imports "groovy-imports" "\
Return a list of all fully-qualified packages in the current Groovy-mode buffer.

\(fn)" t nil)

(autoload 'groovy-imports-add-import-with-package "groovy-imports" "\
Add an import for the class for the name and package. Uses no caching.

\(fn CLASS-NAME PACKAGE)" t nil)

(autoload 'groovy-imports-add-import "groovy-imports" "\
Import the Groovy class for the symbol at point. Uses the symbol
at the point for the class name, ask for a confirmation of the
class name before adding it.

Checks the import cache to see if a package entry exists for the
given class. If found, adds an import statement for the class. If
not found, prompts for the package and saves it to the cache.

If called with a prefix argument, overwrites the package for an
already-existing class name.

\(fn CLASS-NAME)" t nil)

(autoload 'groovy-imports-add-import-dwim "groovy-imports" "\
Add an import statement for the class at point. If no class is
found, prompt for the class name. If the class's package already
exists in the cache, add it and return, otherwise prompt for the
package and cache it for future statements.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "groovy-imports" '("groovy-imports-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; groovy-imports-autoloads.el ends here
