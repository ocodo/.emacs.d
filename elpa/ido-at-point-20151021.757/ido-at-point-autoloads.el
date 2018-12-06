;;; ido-at-point-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "ido-at-point" "ido-at-point.el" (0 0 0 0))
;;; Generated autoloads from ido-at-point.el

(autoload 'ido-at-point-mode "ido-at-point" "\
Global minor mode to use ido for `completion-at-point'.

When called interactively, toggle `ido-at-point-mode'.  With
prefix ARG, enable `ido-at-point-mode' if ARG is positive,
otherwise disable it.

When called from Lisp, enable `ido-at-point-mode' if ARG is
omitted, nil or positive.  If ARG is `toggle', toggle
`ido-at-point-mode'.  Otherwise behave as if called
interactively.

With `ido-at-point-mode' use ido for `completion-at-point'.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ido-at-point" '("ido-at-point-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; ido-at-point-autoloads.el ends here
