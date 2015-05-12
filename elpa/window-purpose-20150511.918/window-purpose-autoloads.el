;;; window-purpose-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "window-purpose" "window-purpose.el" (21841
;;;;;;  22595 39401 0))
;;; Generated autoloads from window-purpose.el

(defvar purpose-mode nil "\
Non-nil if Purpose mode is enabled.
See the command `purpose-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `purpose-mode'.")

(custom-autoload 'purpose-mode "window-purpose" nil)

(autoload 'purpose-mode "window-purpose" "\
Toggle Purpose mode on or off according to the regular rules.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil "window-purpose-configuration" "window-purpose-configuration.el"
;;;;;;  (21841 22595 51401 0))
;;; Generated autoloads from window-purpose-configuration.el

(autoload 'purpose-set-extension-configuration "window-purpose-configuration" "\
Set an extension's entry in `purpose-extended-configuration'.
KEYWORD should be a keyword used to identify the extension.
CONFIG is a `purpose-conf' object containing the extension's purpose
configuration.
Example:
 (purpose-set-extension-configuration
     :python
     (purpose-conf :mode-purposes
                   '((python-mode . python)
                     (python-inferior-mode . interpreter))))

This function calls `purpose-compile-extended-configuration' when its
done.

\(fn KEYWORD CONFIG)" nil nil)

;;;***

;;;### (autoloads nil "window-purpose-x" "window-purpose-x.el" (21841
;;;;;;  22595 39401 0))
;;; Generated autoloads from window-purpose-x.el

(autoload 'purpose-x-code1-setup "window-purpose-x" "\
Setup purpose-x-code1.
This setup includes 4 windows:
1. dedicated 'edit window
2. dedicated 'dired window.  This window shows the current buffer's
directory in a special window, using `dired' and
`dired-hide-details-mode' (if available).
3. dedicated 'buffers window.  This window shows the currently open
files, using `ibuffer'.
4. dedicated 'ilist window.  This window shows the current buffer's
imenu.

\(fn)" t nil)

(autoload 'purpose-x-magit-single-on "window-purpose-x" "\
Turn on magit-single purpose configuration.

\(fn)" t nil)

(autoload 'purpose-x-magit-multi-on "window-purpose-x" "\
Turn on magit-multi purpose configuration.

\(fn)" t nil)

(autoload 'purpose-x-golden-ratio-setup "window-purpose-x" "\
Make `golden-ratio-mode' aware of `purpose-mode'.

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("purpose-x.el" "purpose.el" "window-purpose-core.el"
;;;;;;  "window-purpose-fixes.el" "window-purpose-layout.el" "window-purpose-pkg.el"
;;;;;;  "window-purpose-prefix-overload.el" "window-purpose-switch.el"
;;;;;;  "window-purpose-utils.el") (21841 22595 66011 282000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; window-purpose-autoloads.el ends here
