;;; key-combo-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "key-combo" "key-combo.el" (0 0 0 0))
;;; Generated autoloads from key-combo.el

(autoload 'key-combo-define "key-combo" "\
In KEYMAP, define key sequence KEY as COMMANDS.
KEYMAP is a keymap.

KEY is a string or a vector of symbols and characters meaning a
sequence of keystrokes and events.  Non-ASCII characters with codes
above 127 (such as ISO Latin-1) can be included if you use a vector.

COMMANDS can be an interactive function, a string, nil, or list of these COMMAND.
If COMMANDS is string, treated as a smartchr flavor keyboard macro.
If COMMANDS is nil, the key-chord is removed.
If COMMANDS is list, treated as sequential commands.

\(fn KEYMAP KEY COMMANDS)" nil nil)

(autoload 'key-combo-define-global "key-combo" "\
Give KEY a global binding as COMMAND.

See also `key-combo-define'

Note that if KEY has a local binding in the current buffer,
that local binding will continue to shadow any global binding
that you make with this function.

\(fn KEYS COMMAND)" nil nil)

(autoload 'key-combo-define-local "key-combo" "\
Give KEY a local binding as COMMAND.

See also `key-combo-define'

The binding goes in the current buffer's local map,
which in most cases is shared with all other buffers in the same major mode.

\(fn KEYS COMMAND)" nil nil)

(autoload 'key-combo-define-hook "key-combo" "\


\(fn HOOKS NAME KEYS)" nil t)

(autoload 'key-combo-load-default "key-combo" "\


\(fn)" t nil)

(autoload 'key-combo-mode "key-combo" "\
Toggle key combo.

\(fn &optional ARG)" t nil)

(defvar global-key-combo-mode nil "\
Non-nil if Global Key-Combo mode is enabled.
See the `global-key-combo-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-key-combo-mode'.")

(custom-autoload 'global-key-combo-mode "key-combo" nil)

(autoload 'global-key-combo-mode "key-combo" "\
Toggle Key-Combo mode in all buffers.
With prefix ARG, enable Global Key-Combo mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Key-Combo mode is enabled in all buffers where
`key-combo-mode-maybe' would do it.
See `key-combo-mode' for more information on Key-Combo mode.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "key-combo" '("key-combo" "multiple-cursors-mode" "define-key-combo-load")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; key-combo-autoloads.el ends here
