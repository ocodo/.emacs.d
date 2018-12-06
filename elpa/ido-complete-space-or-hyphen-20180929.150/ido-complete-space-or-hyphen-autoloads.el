;;; ido-complete-space-or-hyphen-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "ido-complete-space-or-hyphen" "ido-complete-space-or-hyphen.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ido-complete-space-or-hyphen.el

(defvar ido-complete-space-or-hyphen-mode nil "\
Non-nil if Ido-Complete-Space-Or-Hyphen mode is enabled.
See the `ido-complete-space-or-hyphen-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `ido-complete-space-or-hyphen-mode'.")

(custom-autoload 'ido-complete-space-or-hyphen-mode "ido-complete-space-or-hyphen" nil)

(autoload 'ido-complete-space-or-hyphen-mode "ido-complete-space-or-hyphen" "\
Toggle ido-complete-space-or-hyphen mode.

The default behavior of ido SPACE key will try to insert SPACE if it makes
sense (a.k.a, the common part of all matches contains SPACE). Howerver,
when ido is used to complete lisp functions or variables, like what smex
does, HYPHEN is used as separator. This extension for ido inserts SPACE or
HYPHEN whenever which one makes sense, just like what built-in M-x does.

You can also temporarily disable ido-complete-space-or-hyphen-mode
within a function by let-binding this to nil:

    (let ((ido-complete-space-or-hyphen-mode nil))
      (ido-completing-read ...))

\(fn &optional ARG)" t nil)

(autoload 'ido-complete-space-or-hyphen-enable "ido-complete-space-or-hyphen" "\
Enable ido-complete-space-or-hyphen

\(fn)" t nil)

(autoload 'ido-complete-space-or-hyphen-disable "ido-complete-space-or-hyphen" "\
Disable ido-complete-space-or-hyphen

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ido-complete-space-or-hyphen" '("ido-complete-space-or-hyphen")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; ido-complete-space-or-hyphen-autoloads.el ends here
