;;; autothemer-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "autothemer" "autothemer.el" (0 0 0 0))
;;; Generated autoloads from autothemer.el

(autoload 'autothemer-deftheme "autothemer" "\
Define a theme NAME with description DESCRIPTION.
A color PALETTE can be used to define let*-like
bindings within both the REDUCED-SPECS and the BODY.

\(fn NAME DESCRIPTION PALETTE REDUCED-SPECS &rest BODY)" nil t)

(autoload 'autothemer-generate-templates "autothemer" "\
Autogenerate customizations for all unthemed faces.
Iterate through all currently defined faces, select those that
have been left uncustomized by the most recent call to
`autothemer-deftheme' and generate customizations that best
approximate the faces' current definitions using the color
palette used in the most recent invocation of
`autothemer-deftheme'.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "autothemer" '("autothemer--")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; autothemer-autoloads.el ends here
