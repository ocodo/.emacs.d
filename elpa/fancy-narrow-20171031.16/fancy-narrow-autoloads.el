;;; fancy-narrow-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "fancy-narrow" "fancy-narrow.el" (0 0 0 0))
;;; Generated autoloads from fancy-narrow.el

(defvar fancy-narrow--beginning nil "\
Beginning position.")

(make-variable-buffer-local 'fancy-narrow--beginning)

(defvar fancy-narrow--end nil "\
End position.")

(make-variable-buffer-local 'fancy-narrow--end)

(autoload 'fancy-narrow-active-p "fancy-narrow" "\
If the current buffer fancy-narrowed?

\(fn)" nil nil)

(autoload 'fancy-narrow-to-region "fancy-narrow" "\
Like `narrow-to-region', except it still displays the unreachable text.

Unlike `narrow-to-region', which completely hides text outside
the narrowed region, this function simply deemphasizes the text,
makes it readonly, and makes it unreachable.

This leads to a much more natural feeling, where the region stays
static (instead of moving up to hide the text above) and is
clearly highlighted with respect to the rest of the buffer.

There is a known bug at the moment, which is that comments and
strings don't deemphasize correctly.

To widen the region again afterwards use `fancy-widen'.

\(fn START END)" t nil)

(autoload 'fancy-widen "fancy-narrow" "\
Undo narrowing from `fancy-narrow-to-region'.

\(fn)" t nil)

(defvar fancy-narrow-mode nil "\
Non-nil if Fancy-Narrow mode is enabled.
See the `fancy-narrow-mode' command
for a description of this minor mode.")

(custom-autoload 'fancy-narrow-mode "fancy-narrow" nil)

(autoload 'fancy-narrow-mode "fancy-narrow" "\
Global minor mode that binds the fancy-narrow functions.

The keys used are the same used by the non-fancy functions.
Binds that are replaced are:
   widen
   narrow-to-region
   narrow-to-defun
   narrow-to-page
   org-narrow-to-block
   org-narrow-to-element
   org-narrow-to-subtree

\(fn &optional ARG)" t nil)

(autoload 'org-fancy-narrow-to-block "fancy-narrow" "\
Like `org-narrow-to-block', except using `fancy-narrow-to-region'.

\(fn)" t nil)

(autoload 'fancy-narrow-to-defun "fancy-narrow" "\
Like `narrow-to-defun', except using `fancy-narrow-to-region'.

\(fn &optional ARG)" t nil)

(autoload 'org-fancy-narrow-to-element "fancy-narrow" "\
Like `org-narrow-to-element', except using `fancy-narrow-to-region'.

\(fn)" t nil)

(autoload 'fancy-narrow-to-page "fancy-narrow" "\
Like `narrow-to-page', except using `fancy-narrow-to-region'.

\(fn &optional ARG)" t nil)

(autoload 'org-fancy-narrow-to-subtree "fancy-narrow" "\
Like `org-narrow-to-subtree', except using `fancy-narrow-to-region'.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "fancy-narrow" '("fancy-narrow-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; fancy-narrow-autoloads.el ends here
