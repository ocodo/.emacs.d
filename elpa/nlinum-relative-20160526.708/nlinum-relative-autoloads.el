;;; nlinum-relative-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "nlinum-relative" "nlinum-relative.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from nlinum-relative.el

(autoload 'nlinum-relative-on "nlinum-relative" "\
Turn ON nlinum-relative.

\(fn)" t nil)

(autoload 'nlinum-relative-off "nlinum-relative" "\
Turn OFF nlinum-relative.

\(fn)" t nil)

(autoload 'nlinum-relative-toggle "nlinum-relative" "\
Toggle between linum-relative and linum.

\(fn)" t nil)

(autoload 'nlinum-relative-mode "nlinum-relative" "\
Display relative line numbers for current buffer.

\(fn &optional ARG)" t nil)

(defvar global-nlinum-relative-mode nil "\
Non-nil if Global Nlinum-Relative mode is enabled.
See the `global-nlinum-relative-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-nlinum-relative-mode'.")

(custom-autoload 'global-nlinum-relative-mode "nlinum-relative" nil)

(autoload 'global-nlinum-relative-mode "nlinum-relative" "\
Toggle Nlinum-Relative mode in all buffers.
With prefix ARG, enable Global Nlinum-Relative mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Nlinum-Relative mode is enabled in all buffers where
`(lambda nil (unless (minibufferp) (nlinum-relative-mode)))' would do it.
See `nlinum-relative-mode' for more information on Nlinum-Relative mode.

\(fn &optional ARG)" t nil)

(autoload 'nlinum-relative-setup-evil "nlinum-relative" "\
Setup nlinum-relative-mode for evil

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "nlinum-relative" '("nlinum-relative-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; nlinum-relative-autoloads.el ends here
