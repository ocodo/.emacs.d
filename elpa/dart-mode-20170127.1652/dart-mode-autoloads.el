;;; dart-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "dart-mode" "dart-mode.el" (0 0 0 0))
;;; Generated autoloads from dart-mode.el

(autoload 'dartfmt-before-save "dart-mode" "\
Add this to .emacs to run dartfmt on the current buffer when saving:
 (add-hook 'before-save-hook 'dartfmt-before-save).

Note that this will cause dart-mode to get loaded the first time
you save any file, kind of defeating the point of autoloading.

\(fn)" t nil)
 (add-to-list 'auto-mode-alist '("\\.dart\\'" . dart-mode))

(autoload 'dart-mode "dart-mode" "\
Major mode for editing Dart files.

The hook `c-mode-common-hook' is run with no args at mode
initialization, then `dart-mode-hook'.

Key bindings:
\\{dart-mode-map}

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "dart-mode" '("dart" "-dart-beginning-of-statement-p" "c-syntactic-context")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; dart-mode-autoloads.el ends here
