;;; helm-swoop-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "helm-swoop" "helm-swoop.el" (0 0 0 0))
;;; Generated autoloads from helm-swoop.el

(autoload 'helm-swoop-back-to-last-point "helm-swoop" "\
Go back to last position where `helm-swoop' was called.
If CANCEL is non-nil, store `helm-swoop-last-point'.

\(fn &optional CANCEL)" t nil)

(autoload 'helm-swoop "helm-swoop" "\
List the all lines to another buffer, which is able to squeeze by
 any words you input. At the same time, the original buffer's cursor
 is jumping line to line according to moving up and down the list.

\(fn &key QUERY SOURCE (MULTILINE current-prefix-arg))" t nil)

(autoload 'helm-swoop-from-isearch "helm-swoop" "\
Invoke `helm-swoop' from isearch." t nil)

(autoload 'helm-multi-swoop "helm-swoop" "\
Multi swoop for QUERY in BUFLIST.

Usage:
  \\[execute-extended-command] helm-multi-swoop
  1. Select any buffers by [C-SPC] or [M-SPC]
  2. Press [RET] to start `helm-multi-swoop'

\\[universal-argument] \\[execute-extended-command] helm-multi-swoop
If you have done helm-multi-swoop before, you can skip select buffers step.
Last selected buffers will be applied to helm-multi-swoop.

\(fn &optional QUERY BUFLIST)" t nil)

(autoload 'helm-multi-swoop-all "helm-swoop" "\
Apply all buffers to helm-multi-swoop for QUERY.

\(fn &optional QUERY)" t nil)

(autoload 'helm-multi-swoop-org "helm-swoop" "\
Applie all `org-mode' buffers to helm-multi-swoop for QUERY.

\(fn &optional QUERY)" t nil)

(autoload 'helm-multi-swoop-current-mode "helm-swoop" "\
Applie all buffers of the same mode as the current buffer to helm-multi-swoop for QUERY.

\(fn &optional QUERY)" t nil)

(autoload 'helm-multi-swoop-projectile "helm-swoop" "\
Apply all opened buffers of the current project to helm-multi-swoop for QUERY.

\(fn &optional QUERY)" t nil)

(autoload 'helm-swoop-without-pre-input "helm-swoop" "\
Start helm-swoop without pre input query." t nil)

(autoload 'helm-swoop-symble-pre-input "helm-swoop" "\
Start helm-swoop without pre input query." t nil)

(autoload 'helm-multi-swoop-edit "helm-swoop" "\
Multi swoop edit." t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-swoop" '("get-buffers-matching-mode" "helm-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; helm-swoop-autoloads.el ends here
