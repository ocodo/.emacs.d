;;; evil-nerd-commenter-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "evil-nerd-commenter" "evil-nerd-commenter.el"
;;;;;;  (21642 20360 363401 0))
;;; Generated autoloads from evil-nerd-commenter.el

(autoload 'evilnc-comment-or-uncomment-paragraphs "evil-nerd-commenter" "\
Comment or uncomment paragraph(s). A paragraph is a continuation non-empty lines.
Paragraphs are separated by empty lines.

\(fn &optional NUM)" t nil)

(autoload 'evilnc-comment-or-uncomment-to-the-line "evil-nerd-commenter" "\
Comment or uncomment from the current line to the LINENUM line

\(fn &optional LINENUM)" t nil)

(autoload 'evilnc-quick-comment-or-uncomment-to-the-line "evil-nerd-commenter" "\
Comment or uncomment to line number by specifying its last digit(s)
For exmaple, you can use 'C-u 53 M-x evilnc-quick-comment-or-uncomment-to-the-line'
or 'C-u 3 M-x evilnc-quick-comment-or-uncomment-to-the-line' to comment to the line 6453

\(fn &optional UNITS)" t nil)

(autoload 'evilnc-toggle-invert-comment-line-by-line "evil-nerd-commenter" "\


\(fn)" t nil)

(autoload 'evilnc-toggle-comment-empty-lines "evil-nerd-commenter" "\


\(fn)" t nil)

(autoload 'evilnc-comment-or-uncomment-lines "evil-nerd-commenter" "\
Comment or uncomment NUM lines. NUM could be negative.
   Case 1: If no region selected, comment/uncomment on current line. if NUM>1, comment/uncomment
   extra N-1 lines from next line
   Case 2: If a region selected, the region is expand to make sure the region contain
   whole lines. Then we comment/uncomment the expanded region. NUM is ignored.

\(fn &optional NUM)" t nil)

(autoload 'evilnc-copy-and-comment-lines "evil-nerd-commenter" "\
Copy and paste NUM lines. Then comment the original lines. NUM could be negative.
   Case 1: If no region selected, operate on current line. if NUM>1, comment/uncomment
   extra N-1 lines from next line
   Case 2: If a region selected, the region is expand to make sure the region contain
   whole lines. Then we operate the expanded region. NUM is ignored.

\(fn &optional NUM)" t nil)

(autoload 'evilnc-copy-to-line "evil-nerd-commenter" "\
Copy from the current line to the LINENUM line, for non-evil user only

\(fn &optional LINENUM)" t nil)

(autoload 'evilnc-kill-to-line "evil-nerd-commenter" "\
Kill from the current line to the LINENUM line, for non-evil user only

\(fn &optional LINENUM)" t nil)

(autoload 'evilnc-version "evil-nerd-commenter" "\


\(fn)" t nil)

(autoload 'evilnc-default-hotkeys "evil-nerd-commenter" "\
Set the hotkeys of evil-nerd-comment

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("evil-nerd-commenter-operator.el" "evil-nerd-commenter-pkg.el")
;;;;;;  (21642 20360 377486 563000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; evil-nerd-commenter-autoloads.el ends here
