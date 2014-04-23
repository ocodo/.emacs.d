;;; clojure-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "clojure-mode" "clojure-mode.el" (21335 7912
;;;;;;  170176 999000))
;;; Generated autoloads from clojure-mode.el

(autoload 'clojure-mode "clojure-mode" "\
Major mode for editing Clojure code.

Commands:
Delete converts tabs to spaces as it moves back.
Blank lines separate paragraphs.
Semicolons start comments.

\\{clojure-mode-map}

Note that `run-lisp' may be used either to start an inferior Lisp
job or to switch back to an existing one.

Entry to this mode calls the value of `clojure-mode-hook' if that
value is non-nil.

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.clj[sx]?\\'" . clojure-mode))

(add-to-list 'auto-mode-alist '("\\.dtm\\'" . clojure-mode))

(add-to-list 'auto-mode-alist '("\\.edn\\'" . clojure-mode))

(add-to-list 'interpreter-mode-alist '("jark" . clojure-mode))

(add-to-list 'interpreter-mode-alist '("cake" . clojure-mode))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; clojure-mode-autoloads.el ends here
