;;; flymake-csslint-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (flymake-csslint-init) "flymake-csslint" "flymake-csslint.el"
;;;;;;  (20774 39324))
;;; Generated autoloads from flymake-csslint.el

(autoload 'flymake-csslint-init "flymake-csslint" "\


\(fn)" nil nil)

(eval-after-load 'flymake '(progn (add-to-list 'flymake-allowed-file-name-masks '(".+\\.css$" flymake-csslint-init flymake-simple-cleanup flymake-get-real-file-name)) (add-to-list 'flymake-err-line-patterns '("^\\(.*\\): line \\([[:digit:]]+\\), col \\([[:digit:]]+\\), \\(.+\\)$" 1 2 3 4)) (add-hook 'css-mode-hook (lambda nil (flymake-mode 1)) t)))

;;;***

;;;### (autoloads nil nil ("flymake-csslint-pkg.el") (20774 39324
;;;;;;  321633))

;;;***

(provide 'flymake-csslint-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; flymake-csslint-autoloads.el ends here
