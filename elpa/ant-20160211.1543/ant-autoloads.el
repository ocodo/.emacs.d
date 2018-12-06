;;; ant-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "ant" "ant.el" (0 0 0 0))
;;; Generated autoloads from ant.el

(autoload 'ant-kill-cache "ant" "\


\(fn)" t nil)

(autoload 'ant "ant" "\
Run ant `task` in project root directory.

\(fn &optional TASK)" t nil)

(autoload 'ant-last "ant" "\
Run the last ant task in project

\(fn)" t nil)

(autoload 'ant-compile "ant" "\


\(fn)" t nil)

(autoload 'ant-clean "ant" "\


\(fn)" t nil)

(autoload 'ant-test "ant" "\


\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ant" '("ant-" "*ant-tasks-c")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; ant-autoloads.el ends here
