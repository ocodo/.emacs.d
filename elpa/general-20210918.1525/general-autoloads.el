;;; general-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "general" "general.el" (0 0 0 0))
;;; Generated autoloads from general.el

(autoload 'general-define-key "general" "\
The primary key definition function provided by general.el.

Define MAPS, optionally using DEFINER, in the keymap(s) corresponding to STATES
and KEYMAPS.

MAPS consists of paired keys (vectors or strings; also see
`general-implicit-kbd') and definitions (those mentioned in `define-key''s
docstring and general.el's \"extended\" definitions). All pairs (when not
ignored) will be recorded and can be later displayed with
`general-describe-keybindings'.

If DEFINER is specified, a custom key definer will be used to bind MAPS. See
general.el's documentation/README for more information.

Unlike with normal key definitions functions, the keymaps in KEYMAPS should be
quoted (this allows using the keymap name for other purposes, e.g. deferring
keybindings if the keymap symbol is not bound, optionally inferring the
corresponding major mode for a symbol by removing \"-map\" for :which-key,
easily storing the keymap name for use with `general-describe-keybindings',
etc.). Note that general.el provides other key definer macros that do not
require quoting keymaps.

STATES corresponds to the evil state(s) to bind the keys in. Non-evil users
should not set STATES. When STATES is non-nil, `evil-define-key*' will be
used (the evil auxiliary keymaps corresponding STATES and KEYMAPS will be used);
otherwise `define-key' will be used (unless DEFINER is specified). KEYMAPS
defaults to 'global. There is also 'local, which create buffer-local
keybindings for both evil and non-evil keybindings. There are other special,
user-alterable \"shorthand\" symbols for keymaps and states (see
`general-keymap-aliases' and `general-state-aliases').

Note that STATES and KEYMAPS can either be lists or single symbols. If any
keymap does not exist, those keybindings will be deferred until the keymap does
exist, so using `eval-after-load' is not necessary with this function.

PREFIX corresponds to a key to prefix keys in MAPS with and defaults to none. To
bind/unbind a key specified with PREFIX, \"\" can be specified as a key in
MAPS (e.g. ...:prefix \"SPC\" \"\" nil... will unbind space).

The keywords in this paragraph are only useful for evil users. If
NON-NORMAL-PREFIX is specified, this prefix will be used instead of PREFIX for
states in `general-non-normal-states' (e.g. the emacs and insert states). This
argument will only have an effect if one of these states is in STATES or if
corresponding global keymap (e.g. `evil-insert-state-map') is in KEYMAPS.
Alternatively, GLOBAL-PREFIX can be used with PREFIX and/or NON-NORMAL-PREFIX to
bind keys in all states under the specified prefix. Like with NON-NORMAL-PREFIX,
GLOBAL-PREFIX will prevent PREFIX from applying to `general-non-normal-states'.
INFIX can be used to append a string to all of the specified prefixes. This is
potentially useful when you are using GLOBAL-PREFIX and/or NON-NORMAL-PREFIX so
that you can sandwich keys in between all the prefixes and the specified keys in
MAPS. This may be particularly useful if you are using default prefixes in a
wrapper function/macro so that you can add to them without needing to re-specify
all of them. If none of the other prefix keyword arguments are specified, INFIX
will have no effect.

If PREFIX-COMMAND or PREFIX-MAP is specified, a prefix command and/or keymap
will be created. PREFIX-NAME can be additionally specified to set the keymap
menu name/prompt. If PREFIX-COMMAND is specified, `define-prefix-command' will
be used. Otherwise, only a prefix keymap will be created. Previously created
prefix commands/keymaps will never be redefined/cleared. All prefixes (including
the INFIX key, if specified) will then be bound to PREFIX-COMMAND or PREFIX-MAP.
If the user did not specify any PREFIX or manually specify any KEYMAPS, general
will bind all MAPS in the prefix keymap corresponding to either PREFIX-MAP or
PREFIX-COMMAND instead of in the default keymap.

PREDICATE corresponds to a predicate to check to determine whether a definition
should be active (e.g. \":predicate '(eobp)\"). Definitions created with a
predicate will only be active when the predicate is true. When the predicate is
false, key lookup will continue to search for a match in lower-precedence
keymaps.

In addition to the normal definitions supported by `define-key', general.el also
provides \"extended\" definitions, which are plists containing the normal
definition as well as other keywords. For example, PREDICATE can be specified
globally or locally in an extended definition. New global (~general-define-key~)
and local (extended definition) keywords can be added by the user. See
`general-extended-def-keywords' and general.el's documentation/README for more
information.

PACKAGE is the global version of the extended definition keyword that specifies
the package a keymap is defined in (used for \"autoloading\" keymaps)

PROPERTIES, REPEAT, and JUMP are the global versions of the extended definition
keywords used for adding evil command properties to commands.

MAJOR-MODES, WK-MATCH-KEYS, WK-MATCH-BINDINGS, and WK-FULL-KEYS are the
corresponding global versions of which-key extended definition keywords. They
will only have an effect for extended definitions that specify :which-key or
:wk. See the section on extended definitions in the general.el
documentation/README for more information.

LISPY-PLIST and WORF-PLIST are the global versions of extended definition
keywords that are used for each corresponding custom DEFINER.

\(fn &rest MAPS &key DEFINER (STATES general-default-states) (KEYMAPS general-default-keymaps KEYMAPS-SPECIFIED-P) (PREFIX general-default-prefix) (NON-NORMAL-PREFIX general-default-non-normal-prefix) (GLOBAL-PREFIX general-default-global-prefix) INFIX PREFIX-COMMAND PREFIX-MAP PREFIX-NAME PREDICATE PACKAGE PROPERTIES REPEAT JUMP MAJOR-MODES (WK-MATCH-KEYS t) (WK-MATCH-BINDING t) (WK-FULL-KEYS t) LISPY-PLIST WORF-PLIST &allow-other-keys)" nil nil)

(autoload 'general-emacs-define-key "general" "\
A wrapper for `general-define-key' that is similar to `define-key'.
It has a positional argument for KEYMAPS (that will not be overridden by a later
:keymaps argument). Besides this, it acts the same as `general-define-key', and
ARGS can contain keyword arguments in addition to keybindings. This can
basically act as a drop-in replacement for `define-key', and unlike with
`general-define-key', KEYMAPS does not need to be quoted.

\(fn KEYMAPS &rest ARGS)" nil t)

(function-put 'general-emacs-define-key 'lisp-indent-function '1)

(autoload 'general-evil-define-key "general" "\
A wrapper for `general-define-key' that is similar to `evil-define-key'.
It has positional arguments for STATES and KEYMAPS (that will not be overridden
by a later :keymaps or :states argument). Besides this, it acts the same as
`general-define-key', and ARGS can contain keyword arguments in addition to
keybindings. This can basically act as a drop-in replacement for
`evil-define-key', and unlike with `general-define-key', KEYMAPS does not need
to be quoted.

\(fn STATES KEYMAPS &rest ARGS)" nil t)

(function-put 'general-evil-define-key 'lisp-indent-function '2)

(autoload 'general-def "general" "\
General definer that takes a variable number of positional arguments in ARGS.
This macro will act as `general-define-key', `general-emacs-define-key', or
`general-evil-define-key' based on how many of the initial arguments do not
correspond to keybindings. All quoted and non-quoted lists and symbols before
the first string, vector, or keyword are considered to be positional arguments.
This means that you cannot use a function or variable for a key that starts
immediately after the positional arguments. If you need to do this, you should
use one of the definers that `general-def' dispatches to or explicitly separate
the positional arguments from the maps with a bogus keyword pair like
\":start-maps t\"

\(fn &rest ARGS)" nil t)

(function-put 'general-def 'lisp-indent-function 'defun)

(autoload 'general-create-definer "general" "\
A helper macro to create wrappers for `general-def'.
This can be used to create key definers that will use a certain keymap, evil
state, prefix key, etc. by default. NAME is the wrapper name and DEFAULTS are
the default arguments. WRAPPING can also be optionally specified to use a
different definer than `general-def'. It should not be quoted.

\(fn NAME &rest DEFAULTS &key WRAPPING &allow-other-keys)" nil t)

(function-put 'general-create-definer 'lisp-indent-function 'defun)

(autoload 'general-defs "general" "\
A wrapper that splits into multiple `general-def's.
Each consecutive grouping of positional argument followed by keyword/argument
pairs (having only one or the other is fine) marks the start of a new section.
Each section corresponds to one use of `general-def'. This means that settings
only apply to the keybindings that directly follow.

Since positional arguments can appear at any point, unqouted symbols are always
considered to be positional arguments (e.g. a keymap). This means that variables
can never be used for keys with `general-defs'. Variables can still be used for
definitions or as arguments to keywords.

\(fn &rest ARGS)" nil t)

(function-put 'general-defs 'lisp-indent-function 'defun)

(autoload 'general-unbind "general" "\
A wrapper for `general-def' to unbind multiple keys simultaneously.
Insert after all keys in ARGS before passing ARGS to `general-def.' \":with
 #'func\" can optionally specified to use a custom function instead (e.g.
 `ignore').

\(fn &rest ARGS)" nil t)

(function-put 'general-unbind 'lisp-indent-function 'defun)

(autoload 'general-describe-keybindings "general" "\
Show all keys that have been bound with general in an org buffer.
Any local keybindings will be shown first followed by global keybindings.
With a non-nil prefix ARG only show bindings in active maps.

\(fn &optional ARG)" t nil)

(autoload 'general-key "general" "\
Act as KEY's definition in the current context.
This uses an extended menu item's capability of dynamically computing a
definition. It is recommended over `general-simulate-key' wherever possible. See
the docstring of `general-simulate-key' and the readme for information about the
benefits and downsides of `general-key'.

KEY should be a string given in `kbd' notation and should correspond to a single
definition (as opposed to a sequence of commands). When STATE is specified, look
up KEY with STATE as the current evil state. When specified, DOCSTRING will be
the menu item's name/description.

Let can be used to bind variables around key lookup. For example:
\(general-key \"some key\"
  :let ((some-var some-val)))

SETUP and TEARDOWN can be used to run certain functions before and after key
lookup. For example, something similar to using :state 'emacs would be:
\(general-key \"some key\"
  :setup (evil-local-mode -1)
  :teardown (evil-local-mode))

ACCEPT-DEFAULT, NO-REMAP, and POSITION are passed to `key-binding'.

\(fn KEY &key STATE DOCSTRING LET SETUP TEARDOWN ACCEPT-DEFAULT NO-REMAP POSITION)" nil t)

(function-put 'general-key 'lisp-indent-function '1)

(autoload 'general-simulate-keys "general" "\
Deprecated. Please use `general-simulate-key' instead.

\(fn KEYS &optional STATE KEYMAP (LOOKUP t) DOCSTRING NAME)" nil t)

(autoload 'general-simulate-key "general" "\
Create and return a command that simulates KEYS in STATE and KEYMAP.

`general-key' should be prefered over this whenever possible as it is simpler
and has saner functionality in many cases because it does not rely on
`unread-command-events' (e.g. \"C-h k\" will show the docstring of the command
to be simulated ; see the readme for more information). The main downsides of
`general-key' are that it cannot simulate a command followed by keys or
subsequent commands, and which-key does not currently work well with it when
simulating a prefix key/incomplete key sequence.

KEYS should be a string given in `kbd' notation. It can also be a list of a
single command followed by a string of the key(s) to simulate after calling that
command. STATE should only be specified by evil users and should be a quoted
evil state. KEYMAP should not be quoted. Both STATE and KEYMAP aliases are
supported (but they have to be set when the macro is expanded). When neither
STATE or KEYMAP are specified, the key(s) will be simulated in the current
context.

If NAME is specified, it will replace the automatically generated function name.
NAME should not be quoted. If DOCSTRING is specified, it will replace the
automatically generated docstring.

Normally the generated function will look up KEY in the correct context to try
to match a command. To prevent this lookup, LOOKUP can be specified as nil.
Generally, you will want to keep LOOKUP non-nil because this will allow checking
the evil repeat property of matched commands to determine whether or not they
should be recorded. See the docstring for `general--simulate-keys' for more
information about LOOKUP.

When a WHICH-KEY description is specified, it will replace the command name in
the which-key popup.

When a command name is specified and that command has been remapped (i.e. [remap
command] is currently bound), the remapped version will be used instead of the
original command unless REMAP is specified as nil (it is true by default).

The advantages of this over a keyboard macro are as follows:
- Prefix arguments are supported
- The user can control the context in which the keys are simulated
- The user can simulate both a named command and keys
- The user can simulate an incomplete key sequence (e.g. for a keymap)

\(fn KEYS &key STATE KEYMAP NAME DOCSTRING (LOOKUP t) WHICH-KEY (REMAP t))" nil t)

(function-put 'general-simulate-key 'lisp-indent-function 'defun)

(autoload 'general-key-dispatch "general" "\
Create and return a command that runs FALLBACK-COMMAND or a command in MAPS.
MAPS consists of <key> <command> pairs. If a key in MAPS is matched, the
corresponding command will be run. Otherwise FALLBACK-COMMAND will be run with
the unmatched keys. So, for example, if \"ab\" was pressed, and \"ab\" is not
one of the key sequences from MAPS, the FALLBACK-COMMAND will be run followed by
the simulated keypresses of \"ab\". Prefix arguments will still work regardless
of which command is run. This is useful for binding under non-prefix keys. For
example, this can be used to redefine a sequence like \"cw\" or \"cow\" in evil
but still have \"c\" work as `evil-change'. If TIMEOUT is specified,
FALLBACK-COMMAND will also be run in the case that the user does not press the
next key within the TIMEOUT (e.g. 0.5).

NAME and DOCSTRING are optional keyword arguments. They can be used to replace
the automatically generated name and docstring for the created function. By
default, `cl-gensym' is used to prevent name clashes (e.g. allows the user to
create multiple different commands using `self-insert-command' as the
FALLBACK-COMMAND without explicitly specifying NAME to manually prevent
clashes).

When INHERIT-KEYMAP is specified, all the keybindings from that keymap will be
inherited in MAPS.

When a WHICH-KEY description is specified, it will replace the command name in
the which-key popup.

When command to be executed has been remapped (i.e. [remap command] is currently
bound), the remapped version will be used instead of the original command unless
REMAP is specified as nil (it is true by default).

\(fn FALLBACK-COMMAND &rest MAPS &key TIMEOUT INHERIT-KEYMAP NAME DOCSTRING WHICH-KEY (REMAP t) &allow-other-keys)" nil t)

(function-put 'general-key-dispatch 'lisp-indent-function '1)

(autoload 'general-predicate-dispatch "general" "\


\(fn FALLBACK-DEF &rest DEFS &key DOCSTRING &allow-other-keys)" nil t)

(function-put 'general-predicate-dispatch 'lisp-indent-function '1)

(autoload 'general-translate-key "general" "\
Translate keys in the keymap(s) corresponding to STATES and KEYMAPS.
STATES should be the name of an evil state, a list of states, or nil. KEYMAPS
should be a symbol corresponding to the keymap to make the translations in or a
list of keymap names. Keymap and state aliases are supported (as well as 'local
and 'global for KEYMAPS).

MAPS corresponds to a list of translations (key replacement pairs). For example,
specifying \"a\" \"b\" will bind \"a\" to \"b\"'s definition in the keymap.
Specifying nil as a replacement will unbind a key.

If DESTRUCTIVE is non-nil, the keymap will be destructively altered without
creating a backup. If DESTRUCTIVE is nil, store a backup of the keymap on the
initial invocation, and for future invocations always look up keys in the
original/backup keymap. On the other hand, if DESTRUCTIVE is non-nil, calling
this function multiple times with \"a\" \"b\" \"b\" \"a\", for example, would
continue to swap and unswap the definitions of these keys. This means that when
DESTRUCTIVE is non-nil, all related swaps/cycles should be done in the same
invocation.

If both MAPS and DESCTRUCTIVE are nil, only create the backup keymap.

\(fn STATES KEYMAPS &rest MAPS &key DESTRUCTIVE &allow-other-keys)" nil nil)

(function-put 'general-translate-key 'lisp-indent-function 'defun)

(autoload 'general-swap-key "general" "\
Wrapper around `general-translate-key' for swapping keys.
STATES, KEYMAPS, and ARGS are passed to `general-translate-key'. ARGS should
consist of key swaps (e.g. \"a\" \"b\" is equivalent to \"a\" \"b\" \"b\" \"a\"
with `general-translate-key') and optionally keyword arguments for
`general-translate-key'.

\(fn STATES KEYMAPS &rest ARGS)" nil t)

(function-put 'general-swap-key 'lisp-indent-function 'defun)

(autoload 'general-auto-unbind-keys "general" "\
Advise `define-key' to automatically unbind keys when necessary.
This will prevent errors when a sub-sequence of a key is already bound (e.g. the
user attempts to bind \"SPC a\" when \"SPC\" is bound, resulting in a \"Key
sequnce starts with non-prefix key\" error). When UNDO is non-nil, remove
advice.

\(fn &optional UNDO)" nil nil)

(autoload 'general-add-hook "general" "\
A drop-in replacement for `add-hook'.
Unlike `add-hook', HOOKS and FUNCTIONS can be single items or lists. APPEND and
LOCAL are passed directly to `add-hook'. When TRANSIENT is non-nil, each
function will remove itself from the hook it is in after it is run once. If
TRANSIENT is a function, call it on the return value in order to determine
whether to remove a function from the hook. For example, if TRANSIENT is
#'identity, remove each function only if it returns non-nil. TRANSIENT could
alternatively check something external and ignore the function's return value.

\(fn HOOKS FUNCTIONS &optional APPEND LOCAL TRANSIENT)" nil nil)

(autoload 'general-remove-hook "general" "\
A drop-in replacement for `remove-hook'.
Unlike `remove-hook', HOOKS and FUNCTIONS can be single items or lists. LOCAL is
passed directly to `remove-hook'.

\(fn HOOKS FUNCTIONS &optional LOCAL)" nil nil)

(autoload 'general-advice-add "general" "\
A drop-in replacement for `advice-add'.
SYMBOLS, WHERE, FUNCTIONS, and PROPS correspond to the arguments for
`advice-add'. Unlike `advice-add', SYMBOLS and FUNCTIONS can be single items or
lists. When TRANSIENT is non-nil, each function will remove itself as advice
after it is run once. If TRANSIENT is a function, call it on the return value in
order to determine whether to remove a function as advice. For example, if
TRANSIENT is #'identity, remove each function only if it returns non-nil.
TRANSIENT could alternatively check something external and ignore the function's
return value.

\(fn SYMBOLS WHERE FUNCTIONS &optional PROPS TRANSIENT)" nil nil)
 (autoload 'general-add-advice "general")

(autoload 'general-advice-remove "general" "\
A drop-in replacement for `advice-remove'.
Unlike `advice-remove', SYMBOLS and FUNCTIONS can be single items or lists.

\(fn SYMBOLS FUNCTIONS)" nil nil)
 (autoload 'general-remove-advice "general")

(autoload 'general-evil-setup "general" "\
Set up some basic equivalents for vim mapping functions.
This creates global key definition functions for the evil states.
Specifying SHORT-NAMES as non-nil will create non-prefixed function
aliases such as `nmap' for `general-nmap'.

\(fn &optional SHORT-NAMES _)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "general" '("general-")))

;;;***

;;;### (autoloads nil nil ("general-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; general-autoloads.el ends here
