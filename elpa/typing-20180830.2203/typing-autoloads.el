;;; typing-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "typing" "typing.el" (0 0 0 0))
;;; Generated autoloads from typing.el

(autoload 'typing-of-emacs "typing" "\
Play the game The Typing Of Emacs.
The game builds a list of words from the current buffer.
In the buffer *The Typing Of Emacs* you will be asked to
type the words.  Should you take more than a certain
number of seconds to do the typing, you loose.  As you
continue playing the words will get longer and longer.

\(fn &optional ZOMBIE)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "typing" '("toe-" "typing-of-emacs-questions")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; typing-autoloads.el ends here
