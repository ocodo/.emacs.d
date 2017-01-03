;;; fix-word-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "fix-word" "fix-word.el" (0 0 0 0))
;;; Generated autoloads from fix-word.el

(autoload 'fix-word "fix-word" "\
Lift function FNC into command that operates on words and regions.

The following behaviors are implemented:

If the point is placed outside of a word, apply FNC to previous
word.  If the command is invoked repeatedly, every its invocation
transforms one more word moving from right to left.  For
example (upcasing, ^ shows position of point/cursor):

  The quick brown fox jumps over the lazy dog.^
  The quick brown fox jumps over the lazy DOG.^
  The quick brown fox jumps over the LAZY DOG.^
  The quick brown fox jumps over THE LAZY DOG.^

The point doesn't move, this allows user to fix recently entered
words and continue typing.

If the point is placed inside any part of a word, the whole word
is transformed.  The point is moved to first character of the
next word.  This allows to transform words repeatedly pressing
dedicated key binding.

  ^The quick brown fox jumps over the lazy dog.
  THE ^quick brown fox jumps over the lazy dog.
  THE QUICK ^brown fox jumps over the lazy dog.
  THE QUICK BROWN ^fox jumps over the lazy dog.

If there is an active region, all words in that region are
transformed.

Use `fix-word' to create new commands like this:

\(defalias 'command-name (fix-word #'upcase)
  \"Description of the command.\")

There is also a macro that defines such commands for you:
`fix-word-define-command'.

\(fn FNC)" nil nil)

(autoload 'fix-word-define-command "fix-word" "\
Define `fix-word'-based command named NAME.

FNC is the processing function and DOC is documentation string.

\(fn NAME FNC &optional DOC)" nil t)

(function-put 'fix-word-define-command 'lisp-indent-function 'defun)

(fix-word-define-command fix-word-upcase #'upcase "Upcase word intelligently.")

(fix-word-define-command fix-word-downcase #'downcase "Downcase word intelligently.")

(fix-word-define-command fix-word-capitalize #'capitalize "Capitalize word intelligently.")

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "fix-word" '("fix-word--")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; fix-word-autoloads.el ends here
