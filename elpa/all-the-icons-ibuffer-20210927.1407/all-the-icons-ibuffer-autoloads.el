;;; all-the-icons-ibuffer-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "all-the-icons-ibuffer" "all-the-icons-ibuffer.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from all-the-icons-ibuffer.el

(defvar all-the-icons-ibuffer-mode nil "\
Non-nil if All-The-Icons-Ibuffer mode is enabled.
See the `all-the-icons-ibuffer-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `all-the-icons-ibuffer-mode'.")

(custom-autoload 'all-the-icons-ibuffer-mode "all-the-icons-ibuffer" nil)

(autoload 'all-the-icons-ibuffer-mode "all-the-icons-ibuffer" "\
Display icons for all buffers in ibuffer.

If called interactively, enable All-The-Icons-Ibuffer mode if ARG
is positive, and disable it if ARG is zero or negative.  If
called from Lisp, also enable the mode if ARG is omitted or nil,
and toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "all-the-icons-ibuffer" '("all-the-icons-ibuffer-" "filename-and-process+" "icon" "mode+" "size-h")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; all-the-icons-ibuffer-autoloads.el ends here
