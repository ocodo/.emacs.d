;;; merlin-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "merlin" "merlin.el" (0 0 0 0))
;;; Generated autoloads from merlin.el

(autoload 'merlin-mode "merlin" "\
Minor mode for interacting with a merlin process.
Runs a merlin process in the background and perform queries on it.

Short cuts:
\\{merlin-mode-map}

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "merlin" '("merlin" "lookup-default" "bounds-of-ocaml-atom-at-point")))

;;;***

;;;### (autoloads nil "merlin-ac" "merlin-ac.el" (0 0 0 0))
;;; Generated autoloads from merlin-ac.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "merlin-ac" '("merlin-ac-" "ac-merlin-locate")))

;;;***

;;;### (autoloads nil "merlin-cap" "merlin-cap.el" (0 0 0 0))
;;; Generated autoloads from merlin-cap.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "merlin-cap" '("merlin-c")))

;;;***

;;;### (autoloads nil "merlin-company" "merlin-company.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from merlin-company.el

(autoload 'merlin-company-backend "merlin-company" "\


\(fn COMMAND &optional ARG &rest IGNORED)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "merlin-company" '("merlin-company--")))

;;;***

;;;### (autoloads nil "merlin-compat" "merlin-compat.el" (0 0 0 0))
;;; Generated autoloads from merlin-compat.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "merlin-compat" '("merlin")))

;;;***

;;;### (autoloads nil "merlin-iedit" "merlin-iedit.el" (0 0 0 0))
;;; Generated autoloads from merlin-iedit.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "merlin-iedit" '("merlin-iedit-")))

;;;***

;;;### (autoloads nil nil ("merlin-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; merlin-autoloads.el ends here
