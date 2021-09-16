;;; dart-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "dart-mode" "dart-mode.el" (0 0 0 0))
;;; Generated autoloads from dart-mode.el
 (add-to-list 'auto-mode-alist '("\\.dart\\'" . dart-mode))

(autoload 'dart-mode "dart-mode" "\
Major mode for editing Dart files.

The hook `dart-mode-hook' is run with no args at mode
initialization.

Key bindings:
\\{dart-mode-map}

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "dart-mode" '("dart-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; dart-mode-autoloads.el ends here
