;;; ido-ubiquitous-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "ido-completing-read+" "ido-completing-read+.el"
;;;;;;  (21800 59914 795401 0))
;;; Generated autoloads from ido-completing-read+.el

(autoload 'ido-completing-read+ "ido-completing-read+" "\
ido-based method for reading from the minibuffer with completion.

See `completing-read' for the meaning of the arguments.

This function is a wrapper for `ido-completing-read' designed to
be used as the value of `completing-read-function'. Importantly,
it detects edge cases that ido cannot handle and uses normal
completion for them.

\(fn PROMPT COLLECTION &optional PREDICATE REQUIRE-MATCH INITIAL-INPUT HIST DEF INHERIT-INPUT-METHOD)" nil nil)

(defadvice ido-completing-read (around ido-cr+ activate) "\
This advice handles application of ido-completing-read+ features.

First, it ensures that `ido-cr+-enable-this-call' is set
properly. This variable should be non-nil during execution of
`ido-completing-read' if it was called from
`ido-completing-read+'.

Second, if `ido-cr+-replace-completely' is non-nil, then this
advice completely replaces `ido-completing-read' with
`ido-completing-read+'." (when (not (featurep (quote ido-completing-read+))) (require (quote ido-completing-read+))) (let ((ido-cr+-enable-this-call ido-cr+-enable-next-call) (ido-cr+-enable-next-call nil)) (if (or ido-cr+-enable-this-call (not ido-cr+-replace-completely)) ad-do-it (message "Replacing ido-completing-read") (setq ad-return-value (apply (function ido-completing-read+) (ad-get-args 0))))))

;;;***

;;;### (autoloads nil "ido-ubiquitous" "ido-ubiquitous.el" (21800
;;;;;;  59914 787401 0))
;;; Generated autoloads from ido-ubiquitous.el

(define-obsolete-variable-alias 'ido-ubiquitous 'ido-ubiquitous-mode "ido-ubiquitous 0.8")

(define-obsolete-function-alias 'ido-ubiquitous 'ido-ubiquitous-mode "ido-ubiquitous 0.8")

(defvar ido-ubiquitous-mode nil "\
Non-nil if Ido-Ubiquitous mode is enabled.
See the command `ido-ubiquitous-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `ido-ubiquitous-mode'.")

(custom-autoload 'ido-ubiquitous-mode "ido-ubiquitous" nil)

(autoload 'ido-ubiquitous-mode "ido-ubiquitous" "\
Use `ido-completing-read' instead of `completing-read' almost everywhere.

If this mode causes problems for a function, you can customize
when ido completion is or is not used by customizing
`ido-ubiquitous-command-overrides' or
`ido-ubiquitous-function-overrides'.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil nil ("ido-ubiquitous-pkg.el") (21800 59914
;;;;;;  807227 888000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; ido-ubiquitous-autoloads.el ends here
