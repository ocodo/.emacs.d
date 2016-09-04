;;; go-playground-cli-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "go-playground-cli" "go-playground-cli.el"
;;;;;;  (22474 20483 0 0))
;;; Generated autoloads from go-playground-cli.el

(autoload 'go-playground-cli-run "go-playground-cli" "\
Compile and run go program from PATH.

\(fn PATH)" t nil)

(autoload 'go-playground-cli-run-current-file "go-playground-cli" "\
Compile and run go program from current file.

\(fn)" t nil)

(with-eval-after-load 'go-mode (define-key (lookup-key go-mode-map [menu-bar Go Playground]) [Run] (progn `("Run" \, #'go-playground-cli-run-current-file))))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; go-playground-cli-autoloads.el ends here
