;;; merlin-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "merlin" "merlin.el" (22246 12258 374503 61000))
;;; Generated autoloads from merlin.el

(autoload 'merlin-mode "merlin" "\
Minor mode for interacting with a merlin process.
Runs a merlin process in the background and perform queries on it.

Short cuts:
\\{merlin-mode-map}

\(fn &optional ARG)" t nil)

(eval-after-load 'company '(require 'merlin-company))

(eval-after-load 'auto-complete '(require 'merlin-ac))

(eval-after-load 'iedit '(require 'merlin-iedit))

;;;***

;;;### (autoloads nil "merlin-company" "merlin-company.el" (22246
;;;;;;  12258 378503 57000))
;;; Generated autoloads from merlin-company.el

(autoload 'merlin-company-backend "merlin-company" "\


\(fn COMMAND &optional ARG &rest IGNORED)" t nil)

(eval-after-load 'company '(add-to-list 'company-backends 'merlin-company-backend))

;;;***

;;;### (autoloads nil nil ("merlin-ac.el" "merlin-cap.el" "merlin-compat.el"
;;;;;;  "merlin-iedit.el" "merlin-pkg.el") (22246 12258 391239 36000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; merlin-autoloads.el ends here
