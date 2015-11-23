;;; swiper-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "ivy" "ivy.el" (22098 25045 633054 674000))
;;; Generated autoloads from ivy.el

(autoload 'ivy-resume "ivy" "\
Resume the last completion session.

\(fn)" t nil)

(autoload 'ivy-completing-read "ivy" "\
Read a string in the minibuffer, with completion.

This is an interface that conforms to `completing-read', so that
it can be used for `completing-read-function'.

PROMPT is a string to prompt with; normally it ends in a colon and a space.
COLLECTION can be a list of strings, an alist, an obarray or a hash table.
PREDICATE limits completion to a subset of COLLECTION.
REQUIRE-MATCH is considered boolean.  See `completing-read'.
INITIAL-INPUT is a string that can be inserted into the minibuffer initially.
_HISTORY is ignored for now.
DEF is the default value.
_INHERIT-INPUT-METHOD is ignored for now.

The history, defaults and input-method arguments are ignored for now.

\(fn PROMPT COLLECTION &optional PREDICATE REQUIRE-MATCH INITIAL-INPUT HISTORY DEF INHERIT-INPUT-METHOD)" nil nil)

(defvar ivy-mode nil "\
Non-nil if Ivy mode is enabled.
See the command `ivy-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `ivy-mode'.")

(custom-autoload 'ivy-mode "ivy" nil)

(autoload 'ivy-mode "ivy" "\
Toggle Ivy mode on or off.
With ARG, turn Ivy mode on if arg is positive, off otherwise.
Turning on Ivy mode will set `completing-read-function' to
`ivy-completing-read'.

Global bindings:
\\{ivy-mode-map}

Minibuffer bindings:
\\{ivy-minibuffer-map}

\(fn &optional ARG)" t nil)

(autoload 'ivy-switch-buffer "ivy" "\
Switch to another buffer.

\(fn)" t nil)

(autoload 'ivy-recentf "ivy" "\
Find a file on `recentf-list'.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "swiper" "swiper.el" (22098 25045 625054 666000))
;;; Generated autoloads from swiper.el

(autoload 'swiper-avy "swiper" "\
Jump to one of the current swiper candidates.

\(fn)" t nil)

(autoload 'swiper-mc "swiper" "\


\(fn)" t nil)

(autoload 'swiper "swiper" "\
`isearch' with an overview.
When non-nil, INITIAL-INPUT is the initial search pattern.

\(fn &optional INITIAL-INPUT)" t nil)

;;;***

;;;### (autoloads nil nil ("colir.el" "ivy-hydra.el" "swiper-pkg.el")
;;;;;;  (22098 25045 643129 259000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; swiper-autoloads.el ends here
