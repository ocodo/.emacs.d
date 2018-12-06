;;; flyspell-lazy-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "flyspell-lazy" "flyspell-lazy.el" (0 0 0 0))
;;; Generated autoloads from flyspell-lazy.el

(let ((loads (get 'flyspell-lazy 'custom-loads))) (if (member '"flyspell-lazy" loads) nil (put 'flyspell-lazy 'custom-loads (cons '"flyspell-lazy" loads))))

(defvar flyspell-lazy-mode nil "\
Non-nil if Flyspell-Lazy mode is enabled.
See the `flyspell-lazy-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `flyspell-lazy-mode'.")

(custom-autoload 'flyspell-lazy-mode "flyspell-lazy" nil)

(autoload 'flyspell-lazy-mode "flyspell-lazy" "\
Turn on flyspell-lazy-mode.

Turning on flyspell-lazy-mode will set up hooks which
change how `flyspell-mode' works, in every buffer for which
flyspell is enabled.

When called interactively with no prefix argument this command
toggles the mode.  With a prefix argument, it enables the mode
if the argument is positive and otherwise disables the mode.

When called from Lisp, this command enables the mode if the
argument is omitted or nil, and toggles the mode if the argument
is 'toggle.

\(fn &optional ARG)" t nil)

(autoload 'flyspell-lazy-check-buffer "flyspell-lazy" "\
Check spelling on the whole buffer, respecting flyspell-lazy settings.

With optional FORCE, force spell-check even on a buffer which
would usually be skipped.

\(fn &optional FORCE)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "flyspell-lazy" '("flyspell-lazy-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; flyspell-lazy-autoloads.el ends here
