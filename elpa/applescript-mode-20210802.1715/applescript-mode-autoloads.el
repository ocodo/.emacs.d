;;; applescript-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "applescript-mode" "applescript-mode.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from applescript-mode.el

(autoload 'applescript-mode "applescript-mode" "\
Major mode for editing AppleScript files." t nil)

(add-to-list 'auto-mode-alist '("\\.\\(applescript\\|scpt\\)\\'" . applescript-mode))

(add-to-list 'interpreter-mode-alist '("osascript" . applescript-mode))

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "applescript-mode" '("applescript-" "as-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; applescript-mode-autoloads.el ends here
