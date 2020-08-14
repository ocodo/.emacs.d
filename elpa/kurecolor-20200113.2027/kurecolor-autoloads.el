;;; kurecolor-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "kurecolor" "kurecolor.el" (0 0 0 0))
;;; Generated autoloads from kurecolor.el

(autoload 'kurecolor-increase-brightness-by-step "kurecolor" "\
Increase brightness on hex color at point (or in region) by step.
Accepts universal argument (X).

\(fn X)" t nil)

(autoload 'kurecolor-decrease-brightness-by-step "kurecolor" "\
Decrease brightness on hex color at point (or in region) by step.
Accepts universal argument (X).

\(fn X)" t nil)

(autoload 'kurecolor-increase-saturation-by-step "kurecolor" "\
Increase saturation on hex color at point (or in region) by step.
Accepts universal argument (X).

\(fn X)" t nil)

(autoload 'kurecolor-decrease-saturation-by-step "kurecolor" "\
Decrease saturation on hex color at point (or in region) by step.
Accepts universal argument (X).

\(fn X)" t nil)

(autoload 'kurecolor-increase-hue-by-step "kurecolor" "\
Increase hue on hex color at point (or in region) by step.
Accepts universal argument (X).

\(fn X)" t nil)

(autoload 'kurecolor-decrease-hue-by-step "kurecolor" "\
Decrease hue on hex color at point (or in region) by step.
Accepts universal argument (X).

\(fn X)" t nil)

(autoload 'kurecolor-set-brightness "kurecolor" "\
Interactively change a COLOR's BRIGHTNESS.

\(fn COLOR BRIGHTNESS)" t nil)

(autoload 'kurecolor-set-saturation "kurecolor" "\
Interactively change a COLOR's SATURATION.

\(fn COLOR SATURATION)" t nil)

(autoload 'kurecolor-set-hue "kurecolor" "\
Interactively change a COLOR's HUE.

\(fn COLOR HUE)" t nil)

(autoload 'kurecolor-hex-hue-group "kurecolor" "\
Given a HEX color.
Insert a list of hexcolors of different hue.

\(fn HEX)" t nil)

(autoload 'kurecolor-hex-sat-group "kurecolor" "\
Given a HEX color.
Insert a list of hexcolors of different saturation (sat).

\(fn HEX)" t nil)

(autoload 'kurecolor-hex-val-group "kurecolor" "\
Given a HEX color.
Insert a list of hexcolors of different brightness (val).

\(fn HEX)" t nil)

(autoload 'kurecolor-cssrgb-at-point-or-region-to-hex "kurecolor" "\
CSS rgb color at point or region to hex rgb." t nil)

(autoload 'kurecolor-hexcolor-at-point-or-region-to-css-rgb "kurecolor" "\
Hex rgb color at point or region to css rgb color." t nil)

(autoload 'kurecolor-hexcolor-at-point-or-region-to-css-rgba "kurecolor" "\
Hex rgb color at point or region to css rgba.
Opacity is always set to 1.0." t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "kurecolor" '("kurecolor-" "to-8bit")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; kurecolor-autoloads.el ends here
