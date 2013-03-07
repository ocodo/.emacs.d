Just add an expression below in your init file :

  (require 'nurumacs)

then scrolling is automatically animated. If minimap is not confortable for
you, evaluate

  (setq nurumacs-map nil)

Command "nurumacs-map-toggle" will toggle minimap immediately, even when
"nurumacs-map" is nil.

When you load this library, "auto-hscroll-mode" is automatically disabled,
and nurumacs-auto-hscroll will work instead. To disable this, evaluate

  (setq nurumacs-auto-hscroll nil)

and to use original auto-hscroll again, evaluate

  (setq auto-hscroll-mode t)
