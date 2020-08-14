;;; homebrew-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "homebrew-mode" "homebrew-mode.el" (0 0 0 0))
;;; Generated autoloads from homebrew-mode.el

(autoload 'homebrew-mode "homebrew-mode" "\
Helper functions for editing Homebrew formulae

If called interactively, enable Homebrew mode if ARG is positive,
and disable it if ARG is zero or negative.  If called from Lisp,
also enable the mode if ARG is omitted or nil, and toggle it if
ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(put 'global-homebrew-mode 'globalized-minor-mode t)

(defvar global-homebrew-mode nil "\
Non-nil if Global Homebrew mode is enabled.
See the `global-homebrew-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-homebrew-mode'.")

(custom-autoload 'global-homebrew-mode "homebrew-mode" nil)

(autoload 'global-homebrew-mode "homebrew-mode" "\
Toggle Homebrew mode in all buffers.
With prefix ARG, enable Global Homebrew mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Homebrew mode is enabled in all buffers where
`(lambda nil (if (homebrew--formula-file-p (current-buffer)) (homebrew-mode)))' would do it.
See `homebrew-mode' for more information on Homebrew mode.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "homebrew-mode" '("homebrew-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; homebrew-mode-autoloads.el ends here
