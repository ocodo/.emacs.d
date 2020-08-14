;;; ace-mc-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "ace-mc" "ace-mc.el" (0 0 0 0))
;;; Generated autoloads from ace-mc.el

(autoload 'ace-mc-add-multiple-cursors "ace-mc" "\
Use AceJump to add or remove multiple cursors.

`ace-mc-add-multiple-cursors' will prompt your for locations to add
multiple cursors.  If a cursor already exists at that location,
it will be removed.  This process continues looping until you
exit, for example by pressing return or escape.

Without a \\[universal-argument] prefix argument, use the default
AceJump jumping mode as described in
`ace-jump-mode-submode-list'.  When called interactively with one
or more \\[universal-argument] prefix arguments PREFIX, use the
corresponding mode from `ace-jump-mode-submode-list'.  For
example, by default
   \\[ace-mc-add-multiple-cursors] ==> `ace-jump-word-mode'
   \\[universal-argument] \\[ace-mc-add-multiple-cursors] ==> `ace-jump-char-mode'
   \\[universal-argument] \\[universal-argument] \\[ace-mc-add-multiple-cursors] ==> `ace-jump-line-mode'

If SINGLE-MODE is set to 't', don't loop.

When the region is active, prompt for AceJump matches based on matching strings.

\(fn &optional PREFIX SINGLE-MODE)" t nil)

(autoload 'ace-mc-add-single-cursor "ace-mc" "\
Add a single multiple cursor.

This is a wrapper for `ace-mc-add-multiple-cursors', only adding
a single cursor.

PREFIX is passed to `ace-mc-add-multiple-cursors', see the
documentation there.

\(fn &optional PREFIX)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ace-mc" '("ace-mc-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; ace-mc-autoloads.el ends here
