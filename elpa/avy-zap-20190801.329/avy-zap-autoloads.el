;;; avy-zap-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "avy-zap" "avy-zap.el" (0 0 0 0))
;;; Generated autoloads from avy-zap.el

(autoload 'avy-zap-to-char "avy-zap" "\
Zap to char using `avy'." t nil)

(autoload 'avy-zap-to-char-dwim "avy-zap" "\
Without PREFIX, call `avy-zap-to-char'.
With PREFIX, call `zap-to-char'.

\(fn &optional PREFIX)" t nil)

(autoload 'avy-zap-up-to-char "avy-zap" "\
Zap up to char using `avy'." t nil)

(autoload 'avy-zap-up-to-char-dwim "avy-zap" "\
Without PREFIX, call `avy-zap-up-to-char'.
With PREFIX, call `zap-up-to-char'.

\(fn &optional PREFIX)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "avy-zap" '("avy-zap-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; avy-zap-autoloads.el ends here
