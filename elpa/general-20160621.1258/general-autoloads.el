;;; general-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "general" "general.el" (22488 34895 0 0))
;;; Generated autoloads from general.el

(autoload 'general-define-key "general" "\
The primary key definition function provided by general.

PREFIX corresponds to a prefix key and defaults to none. STATES corresponds to
the evil state(s) to bind the keys in. Non-evil users should not set STATES.
When STATES is non-nil, `evil-define-key' will be used. Otherwise `define-key'
will be used. Evil users may also want to leave STATES nil and set KEYMAPS to
a keymap such as `evil-normal-state-map' for global mappings. KEYMAPS defaults
to `global-map'. Note that STATES and KEYMAPS can either be a list or a single
symbol. If any keymap does not exist, the keybindings will be deferred until
the keymap does exist, so using `eval-after-load' is not necessary with this
function.

If NON-NORMAL-PREFIX is specified, this prefix will be used for emacs and insert
state keybindings instead of PREFIX. This argument will only have an effect if
'insert and/or 'emacs is one of the STATES or if 'evil-insert-state-map and/or
'evil-emacs-state-map is one of the KEYMAPS. Alternatively, GLOBAL-PREFIX can be
used with PREFIX and/or NON-NORMAL-PREFIX to bind keys in all states under a
specified prefix. Like with NON-NORMAL-PREFIX, GLOBAL-PREFIX will prevent PREFIX
from applying to insert and emacs states. Note that these keywords are only
useful for evil users.

INFIX can be used to append a string to all of the specified prefixes. This is
potentially useful when you are using GLOBAL-PREFIX and/or NON-NORMAL-PREFIX so
that you can sandwich keys in between all the prefixes and the specified keys in
MAPS. This may be particularly useful if you are using default prefixes in a
wrapper so that you can add to them without needing to re-specify all of them.
If none of the other prefix arguments are specified, INFIX will have no effect.

Unlike with normal key definitions functions, the keymaps in KEYMAPS should be
quoted (this makes it easy to check if there is only one keymap instead of a
list of keymaps).

MAPS will be recorded for later use with `general-describe-keybindings'.

\(fn &rest MAPS &key (PREFIX general-default-prefix) (NON-NORMAL-PREFIX general-default-non-normal-prefix) (GLOBAL-PREFIX general-default-global-prefix) (INFIX nil) (STATES general-default-states) (KEYMAPS general-default-keymaps) (PREDICATE nil) (PACKAGE nil) (MAJOR-MODE nil) &allow-other-keys)" nil nil)

(autoload 'general-create-definer "general" "\
A helper macro to create key definitions functions.
This allows the creation of key definition functions that
will use a certain keymap, evil state, and/or prefix key by default.
NAME will be the function name and ARGS are the keyword arguments that
are intended to be the defaults.

\(fn NAME &rest ARGS)" nil t)

(autoload 'general-emacs-define-key "general" "\
A wrapper for `general-define-key' that is similar to `define-key'.
It has a positional argument for KEYMAPS. It acts the same as
`general-define-key', and ARGS can contain keyword arguments in addition to
keybindings. This can basically act as a drop-in replacement for `define-key',
and unlike with `general-define-key', if KEYMAPS is a single keymap, it does
not need to be quoted.

\(fn KEYMAPS &rest ARGS)" nil t)

(function-put 'general-emacs-define-key 'lisp-indent-function '1)

(autoload 'general-evil-define-key "general" "\
A wrapper for `general-define-key' that is similar to `evil-define-key'.
It has positional arguments for STATES and KEYMAPS. It acts the same as
`general-define-key', and ARGS can contain keyword arguments in addition to
keybindings. This can basically act as a drop-in replacement for
`evil-define-key', and unlike with `general-define-key', if KEYMAPS is a single
keymap, it does not need to be quoted.

\(fn STATES KEYMAPS &rest ARGS)" nil t)

(function-put 'general-evil-define-key 'lisp-indent-function '2)

(autoload 'general-describe-keybindings "general" "\
Show all keys that have been bound with general in an org buffer.
Any local keybindings will be shown first followed by global keybindings.

\(fn)" t nil)

(autoload 'general-simulate-keys "general" "\
Create a function to simulate KEYS.
If EMACS-STATE is non-nil, execute the keys in emacs state. Otherwise simulate
the keys in the current context (will work without evil). KEYS should be a
string  given in `kbd' notation. It an also be a list of a single command
followed by a string of the keys to simulate after calling that command. If
DOCSTRING is given, it will replace the automatically generated docstring. If
NAME is given, it will replace the automatically generated function name. NAME
should not be quoted.

\(fn KEYS &optional EMACS-STATE DOCSTRING NAME)" nil t)

(autoload 'general-key-dispatch "general" "\
Create a function that will run FALLBACK-COMMAND or a command from MAPS.
MAPS consists of <key> <command> pairs. If a key in MAPS is matched, the
corresponding command will be run. Otherwise FALLBACK-COMMAND will be run
with the unmatched keys. So, for example, if \"ab\" was pressed, and \"ab\" is
not one of the key sequences from MAPS, the FALLBACK-COMMAND will be run
followed by the simulated keypresses of \"ab\". Prefix arguments will still work
regardless of which command is run. This is useful for binding under non-prefix
keys. For example, this can be used to redefine a sequence like \"cw\" or
\"cow\" in evil but still have \"c\" work as `evil-change'. LAMBDA, NAME, and
DOCSTRING are optional keyword arguments. They can be used to replace the
automatically generated name and docstring for the created function and are
potentially useful if you want to create multiple, different commands using the
same FALLBACK-COMMAND (e.g. `self-insert-command').

\(fn FALLBACK-COMMAND &rest MAPS &key NAME DOCSTRING &allow-other-keys)" nil t)

(function-put 'general-key-dispatch 'lisp-indent-function '1)

(autoload 'general-create-vim-definer "general" "\
A helper function to create vim-like wrappers over `general-define-key'.
The function created will be called NAME and will have the keymaps default to
KEYMAPS or the states default to STATES. If DEFAULT-TO-STATES is non-nil,
:states STATES will be used. Otherwise :keymaps KEYMAPS will be used. This can
be overriden later by setting the global `general-vim-definer-default'
option.

\(fn NAME KEYMAPS &optional STATES DEFAULT-TO-STATES)" nil t)

(autoload 'general-create-dual-vim-definer "general" "\
Like `general-create-vim-definer', create a \"vim definer\" called NAME.
Only the short names in the STATES list need to be specified, but this will only
work for valid evil states.

\(fn NAME STATES &optional DEFAULT-TO-STATES)" nil t)

(autoload 'general-evil-setup "general" "\
Set up some basic equivalents for vim mapping functions.
This creates global key definition functions for the evil states.
Specifying SHORT-NAMES as non-nil will create non-prefixed function
aliases such as `nmap' for `general-nmap'.

\(fn &optional SHORT-NAMES DEFAULT-TO-STATES)" nil t)

;;;***

;;;### (autoloads nil nil ("elpa.el" "general-pkg.el") (22488 34895
;;;;;;  0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; general-autoloads.el ends here
