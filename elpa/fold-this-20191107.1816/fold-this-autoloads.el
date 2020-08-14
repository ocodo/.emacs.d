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
folded region.  If not, default to `fold-this-overlay-text'.

\(fn BEG END &optional FOLD-HEADER)" t nil)

(autoload 'fold-this-sexp "fold-this" "\
Fold sexp around point.

If the point is at a symbol, fold the parent sexp.  If the point
is in front of a sexp, fold the following sexp." t nil)

(autoload 'fold-this-all "fold-this" "\
Fold  all occurences of text in region.

\(fn BEG END)" t nil)

(autoload 'fold-this-mode "fold-this" "\
Toggle folding on or off.
With folding activated add custom map \\[fold-this-keymap]

If called interactively, enable Fold-This mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(defvar fold-this-persistent-mode nil "\
Non-nil if Fold-This-Persistent mode is enabled.
See the `fold-this-persistent-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `fold-this-persistent-mode'.")

(custom-autoload 'fold-this-persistent-mode "fold-this" nil)

(autoload 'fold-this-persistent-mode "fold-this" "\
Enable persistence of overlays for `fold-this-mode'

If called interactively, enable Fold-This-Persistent mode if ARG
is positive, and disable it if ARG is zero or negative.  If
called from Lisp, also enable the mode if ARG is omitted or nil,
and toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "fold-this" '("fold-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; fold-this-autoloads.el ends here
