;;; ivy-explorer-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "ivy-explorer" "ivy-explorer.el" (0 0 0 0))
;;; Generated autoloads from ivy-explorer.el

(defvar ivy-explorer-mode nil "\
Non-nil if Ivy-Explorer mode is enabled.
See the `ivy-explorer-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `ivy-explorer-mode'.")

(custom-autoload 'ivy-explorer-mode "ivy-explorer" nil)

(autoload 'ivy-explorer-mode "ivy-explorer" "\
Globally enable `ivy-explorer' for file navigation.

If called interactively, enable Ivy-Explorer mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

`ivy-explorer-mode' is a global minor mode which changes
`read-file-name-function' which is used for file completion.

When `ivy-explorer-enable-counsel-explorer' (by default it is),
`find-file' and `counsel-find-file' will be remapped to
`counsel-explorer.', too.

See `ivy-explorer-map' for bindings used in the minibuffer.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ivy-explorer" '("counsel-explorer" "ivy-explorer")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; ivy-explorer-autoloads.el ends here
