;;; mag-menu-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "mag-menu" "mag-menu.el" (0 0 0 0))
;;; Generated autoloads from mag-menu.el

(autoload 'mag-menu-remove-option "mag-menu" "\


\(fn OPTIONS-ALIST NAME)" nil nil)

(autoload 'mag-menu-set-option "mag-menu" "\


\(fn OPTIONS-ALIST NAME VAL)" nil nil)

(autoload 'mag-menu-toggle-switch "mag-menu" "\


\(fn OPTIONS-ALIST NAME)" nil nil)

(autoload 'mag-menu-add-argument "mag-menu" "\


\(fn GROUP ARG-NAME CALLBACK HISTORY-VAR)" nil nil)

(autoload 'mag-menu "mag-menu" "\
Brings up a menu for the user to select options and then run
actions, all of which are described by GROUP. GROUP should have
the following form:

  `(group-name
     (man-page \"man-page\")
     (actions
      (\"r\" \"Run the command\" action-callback))
     (switches
      (\"-b\" \"Some on/off option\" \"--long-form-option-name\" switch-callback))
     (arguments
      (\"-f\" \"Some option that takes a value\" \"--value=\" arg-callback history-var)))

The group-name value is a symbol describing the program whose
options are being set (e.g. 'ack, 'git-log, etc). It's currently
unused, but may be used in the future. Set it to something
meaningful.

Actions represent commands that can be run, switches are simple
flags that the command can take, and arguments are options that
take a value. Each of these can have multiple entries, although
only one entry for each is used in the example above. The short
name of each entry will be bound as a key in the mag-menu buffer,
which will show up at the bottom of the frame. So in the example
above, \"r\", \"-b\", and \"-f\" will all be bound as keyboard
shortcuts.

The optional OPTIONS-ALIST arg is an assoc list containing
default values for the switches and arguments. The long form
names should be used. An example would be

  '((\"--switch1\") (\"--switch2\") (\"--argument1\" . \"value1\"))

As with any assoc list, each element is a cons pair. Switches
should appear in the cons pair alone, and for arguments, the car
is the argument name (again, the long form name) and the cdr is
the value.

The switch-callback function is optional. If present, it should
be a function that takes two arguments: the first is the option
name (long form), and the second is the current assoc list of
options. The callback should return an options assoc list with
the appropriate changes made. You may find the
mag-menu-toggle-switch function useful for toggling a switch
value. If you don't provide a callback, the switch is simply
toggled. Providing a callback is useful for example if some
switches are mutually exclusive, and you want to disable switch A
when switch B is activated.

Unlike the switch-callback, the arg-callback is mandatory. It
should be a function that takes three arguments: the option name,
the options assoc list, and a history variable. It should prompt
the user for the option value, then return an options assoc list
with the appropriate changes made. Mag-menu provides some
predefined callbacks are provided that may be suitable:
mag-menu-read-generic, mag-menu-read-directory-name, etc.

The action-callback is also mandatory. It should take just one
value, the options assoc list. It's recommended to copy this list
to a separate variable (via copy-tree), and then pass this
variable in as the OPTIONS-ALIST variable the next time you call
mag-menu.

You may want to look at ack-menu.el
\(https://github.com/chumpage/ack-menu) for a complete example of
how to use mag-menu.

\(fn GROUP &optional OPTIONS-ALIST)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "mag-menu" '("mag-menu-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; mag-menu-autoloads.el ends here
