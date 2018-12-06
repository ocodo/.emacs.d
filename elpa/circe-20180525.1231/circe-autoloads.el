;;; circe-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "circe" "circe.el" (0 0 0 0))
;;; Generated autoloads from circe.el

(autoload 'circe-version "circe" "\
Display Circe's version.

\(fn)" t nil)

(autoload 'circe "circe" "\
Connect to IRC.

Connect to the given network specified by NETWORK-OR-SERVER.

When this function is called, it collects options from the
SERVER-OPTIONS argument, the user variable
`circe-network-options', and the defaults found in
`circe-network-defaults', in this order.

If NETWORK-OR-SERVER is not found in any of these variables, the
argument is assumed to be the host name for the server, and all
relevant settings must be passed via SERVER-OPTIONS.

All SERVER-OPTIONS are treated as variables by getting the string
\"circe-\" prepended to their name. This variable is then set
locally in the server buffer.

See `circe-network-options' for a list of common options.

\(fn NETWORK-OR-SERVER &rest SERVER-OPTIONS)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "circe" '("circe-" "lui-logging-format-arguments" "with-circe-server-buffer")))

;;;***

;;;### (autoloads nil "circe-chanop" "circe-chanop.el" (0 0 0 0))
;;; Generated autoloads from circe-chanop.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "circe-chanop" '("circe-command-")))

;;;***

;;;### (autoloads nil "circe-color-nicks" "circe-color-nicks.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from circe-color-nicks.el

(autoload 'enable-circe-color-nicks "circe-color-nicks" "\
Enable the Color Nicks module for Circe.
This module colors all encountered nicks in a cross-server fashion.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "circe-color-nicks" '("circe-" "remove-circe-color-nicks" "add-circe-color-nicks" "disable-circe-color-nicks")))

;;;***

;;;### (autoloads nil "circe-display-images" "circe-display-images.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from circe-display-images.el

(autoload 'enable-circe-display-images "circe-display-images" "\
Enable the Display Images module for Circe.
This module displays various image types when they are linked in a channel

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "circe-display-images" '("circe-display-images" "remove-circe-display-images" "add-circe-display-images" "disable-circe-display-images")))

;;;***

;;;### (autoloads nil "circe-lagmon" "circe-lagmon.el" (0 0 0 0))
;;; Generated autoloads from circe-lagmon.el

(defvar circe-lagmon-mode nil "\
Non-nil if Circe-Lagmon mode is enabled.
See the `circe-lagmon-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `circe-lagmon-mode'.")

(custom-autoload 'circe-lagmon-mode "circe-lagmon" nil)

(autoload 'circe-lagmon-mode "circe-lagmon" "\
Circe-lagmon-mode monitors the amount of lag on your
connection to each server, and displays the lag time in seconds
in the mode-line.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "circe-lagmon" '("circe-lagmon-")))

;;;***

;;;### (autoloads nil "circe-new-day-notifier" "circe-new-day-notifier.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from circe-new-day-notifier.el

(autoload 'enable-circe-new-day-notifier "circe-new-day-notifier" "\


\(fn)" t nil)

(autoload 'disable-circe-new-day-notifier "circe-new-day-notifier" "\


\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "circe-new-day-notifier" '("circe-new-day-notifi")))

;;;***

;;;### (autoloads nil "irc" "irc.el" (0 0 0 0))
;;; Generated autoloads from irc.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "irc" '("irc-")))

;;;***

;;;### (autoloads nil "lcs" "lcs.el" (0 0 0 0))
;;; Generated autoloads from lcs.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lcs" '("lcs-")))

;;;***

;;;### (autoloads nil "lui" "lui.el" (0 0 0 0))
;;; Generated autoloads from lui.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lui" '("lui-")))

;;;***

;;;### (autoloads nil "lui-autopaste" "lui-autopaste.el" (0 0 0 0))
;;; Generated autoloads from lui-autopaste.el

(autoload 'enable-lui-autopaste "lui-autopaste" "\
Enable the lui autopaste feature.

If you enter more than `lui-autopaste-lines' at once, Lui will
ask if you would prefer to use a paste service instead. If you
agree, Lui will paste your input to `lui-autopaste-function' and
replace it with the resulting URL.

\(fn)" t nil)

(autoload 'disable-lui-autopaste "lui-autopaste" "\
Disable the lui autopaste feature.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lui-autopaste" '("lui-autopaste")))

;;;***

;;;### (autoloads nil "lui-format" "lui-format.el" (0 0 0 0))
;;; Generated autoloads from lui-format.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lui-format" '("lui-")))

;;;***

;;;### (autoloads nil "lui-irc-colors" "lui-irc-colors.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from lui-irc-colors.el

(autoload 'enable-lui-irc-colors "lui-irc-colors" "\
Enable IRC color interpretation for Lui.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lui-irc-colors" '("lui-irc-" "disable-lui-irc-colors")))

;;;***

;;;### (autoloads nil "lui-logging" "lui-logging.el" (0 0 0 0))
;;; Generated autoloads from lui-logging.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lui-logging" '("lui-" "disable-lui-logging" "enable-lui-logging")))

;;;***

;;;### (autoloads nil "lui-track-bar" "lui-track-bar.el" (0 0 0 0))
;;; Generated autoloads from lui-track-bar.el

(autoload 'enable-lui-track-bar "lui-track-bar" "\
Enable a bar in Lui buffers that shows where you stopped reading.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lui-track-bar" '("lui-track-bar-")))

;;;***

;;;### (autoloads nil "make-tls-process" "make-tls-process.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from make-tls-process.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "make-tls-process" '("tls-" "make-tls-process")))

;;;***

;;;### (autoloads nil "shorten" "shorten.el" (0 0 0 0))
;;; Generated autoloads from shorten.el

(autoload 'shorten-strings "shorten" "\
Takes a list of strings and returns an alist ((STRING
. SHORTENED-STRING) ...).  Uses `shorten-split-function' to split
the strings, and `shorten-join-function' to join shortened
components back together into SHORTENED-STRING.  See also
`shorten-validate-component-function'.

\(fn STRINGS)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "shorten" '("shorten-")))

;;;***

;;;### (autoloads nil "tracking" "tracking.el" (0 0 0 0))
;;; Generated autoloads from tracking.el

(defvar tracking-mode nil "\
Non-nil if Tracking mode is enabled.
See the `tracking-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `tracking-mode'.")

(custom-autoload 'tracking-mode "tracking" nil)

(autoload 'tracking-mode "tracking" "\
Allow cycling through modified buffers.
This mode in itself does not track buffer modification, but
provides an API for programs to add buffers as modified (using
`tracking-add-buffer').

Once this mode is active, modified buffers are shown in the mode
line. The user can cycle through them using
\\[tracking-next-buffer].

\(fn &optional ARG)" t nil)

(autoload 'tracking-add-buffer "tracking" "\
Add BUFFER as being modified with FACES.
This does check whether BUFFER is currently visible.

If FACES is given, it lists the faces that might be appropriate
for BUFFER in the mode line. The highest-priority face of these
and the current face of the buffer, if any, is used. Priority is
decided according to `tracking-faces-priorities'.
When `tracking-sort-faces-first' is non-nil, all buffers with any
face set will be stable-sorted before any buffers with no face set.

\(fn BUFFER &optional FACES)" nil nil)

(autoload 'tracking-remove-buffer "tracking" "\
Remove BUFFER from being tracked.

\(fn BUFFER)" nil nil)

(autoload 'tracking-next-buffer "tracking" "\
Switch to the next active buffer.

\(fn)" t nil)

(autoload 'tracking-previous-buffer "tracking" "\
Switch to the last active buffer.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "tracking" '("tracking-")))

;;;***

;;;### (autoloads nil nil ("circe-compat.el" "circe-pkg.el") (0 0
;;;;;;  0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; circe-autoloads.el ends here
