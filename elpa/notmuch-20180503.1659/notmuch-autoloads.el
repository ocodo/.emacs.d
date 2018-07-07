;;; notmuch-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "notmuch" "notmuch.el" (23360 14569 950874
;;;;;;  711000))
;;; Generated autoloads from notmuch.el

(autoload 'notmuch-search "notmuch" "\
Display threads matching QUERY in a notmuch-search buffer.

If QUERY is nil, it is read interactively from the minibuffer.
Other optional parameters are used as follows:

  OLDEST-FIRST: A Boolean controlling the sort order of returned threads
  TARGET-THREAD: A thread ID (without the thread: prefix) that will be made
                 current if it appears in the search results.
  TARGET-LINE: The line number to move to if the target thread does not
               appear in the search results.
  NO-DISPLAY: Do not try to foreground the search results buffer. If it is
              already foregrounded i.e. displayed in a window, this has no
              effect, meaning the buffer will remain visible.

When called interactively, this will prompt for a query and use
the configured default sort order.

\(fn &optional QUERY OLDEST-FIRST TARGET-THREAD TARGET-LINE NO-DISPLAY)" t nil)

(autoload 'notmuch "notmuch" "\
Run notmuch and display saved searches, known tags, etc.

\(fn)" t nil)

(autoload 'notmuch-cycle-notmuch-buffers "notmuch" "\
Cycle through any existing notmuch buffers (search, show or hello).

If the current buffer is the only notmuch buffer, bury it. If no
notmuch buffers exist, run `notmuch'.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "notmuch-company" "notmuch-company.el" (23360
;;;;;;  14569 958874 901000))
;;; Generated autoloads from notmuch-company.el

(autoload 'notmuch-company-setup "notmuch-company" "\


\(fn)" nil nil)

(autoload 'notmuch-company "notmuch-company" "\
`company-mode' completion back-end for `notmuch'.

\(fn COMMAND &optional ARG &rest IGNORE)" t nil)

;;;***

;;;### (autoloads nil "notmuch-hello" "notmuch-hello.el" (23360 14569
;;;;;;  954874 806000))
;;; Generated autoloads from notmuch-hello.el

(autoload 'notmuch-hello "notmuch-hello" "\
Run notmuch and display saved searches, known tags, etc.

\(fn &optional NO-DISPLAY)" t nil)

;;;***

;;;### (autoloads nil "notmuch-jump" "notmuch-jump.el" (23360 14569
;;;;;;  942874 521000))
;;; Generated autoloads from notmuch-jump.el

(autoload 'notmuch-jump-search "notmuch-jump" "\
Jump to a saved search by shortcut key.

This prompts for and performs a saved search using the shortcut
keys configured in the :key property of `notmuch-saved-searches'.
Typically these shortcuts are a single key long, so this is a
fast way to jump to a saved search from anywhere in Notmuch.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "notmuch-show" "notmuch-show.el" (23360 14569
;;;;;;  958874 901000))
;;; Generated autoloads from notmuch-show.el

(autoload 'notmuch-show "notmuch-show" "\
Run \"notmuch show\" with the given thread ID and display results.

ELIDE-TOGGLE, if non-nil, inverts the default elide behavior.

The optional PARENT-BUFFER is the notmuch-search buffer from
which this notmuch-show command was executed, (so that the
next thread from that buffer can be show when done with this
one).

The optional QUERY-CONTEXT is a notmuch search term. Only
messages from the thread matching this search term are shown if
non-nil.

The optional BUFFER-NAME provides the name of the buffer in
which the message thread is shown. If it is nil (which occurs
when the command is called interactively) the argument to the
function is used.

Returns the buffer containing the messages, or NIL if no messages
matched.

\(fn THREAD-ID &optional ELIDE-TOGGLE PARENT-BUFFER QUERY-CONTEXT BUFFER-NAME)" t nil)

;;;***

;;;### (autoloads nil nil ("coolj.el" "make-deps.el" "notmuch-address.el"
;;;;;;  "notmuch-compat.el" "notmuch-crypto.el" "notmuch-draft.el"
;;;;;;  "notmuch-lib.el" "notmuch-maildir-fcc.el" "notmuch-message.el"
;;;;;;  "notmuch-mua.el" "notmuch-parser.el" "notmuch-pkg.el" "notmuch-print.el"
;;;;;;  "notmuch-query.el" "notmuch-tag.el" "notmuch-tree.el" "notmuch-wash.el")
;;;;;;  (23360 14569 962874 996000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; notmuch-autoloads.el ends here
