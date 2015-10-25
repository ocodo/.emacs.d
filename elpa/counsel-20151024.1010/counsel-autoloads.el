;;; counsel-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "counsel" "counsel.el" (22060 27457 220194
;;;;;;  486000))
;;; Generated autoloads from counsel.el

(autoload 'counsel-el "counsel" "\
Elisp completion at point.

\(fn)" t nil)

(autoload 'counsel-cl "counsel" "\
Common Lisp completion at point.

\(fn)" t nil)

(autoload 'counsel-describe-variable "counsel" "\
Forward to `describe-variable'.

\(fn)" t nil)

(autoload 'counsel-describe-function "counsel" "\
Forward to `describe-function'.

\(fn)" t nil)

(autoload 'counsel-info-lookup-symbol "counsel" "\
Forward to (`info-describe-symbol' SYMBOL MODE) with ivy completion.

\(fn SYMBOL &optional MODE)" t nil)

(autoload 'counsel-unicode-char "counsel" "\
Insert a Unicode character at point.

\(fn)" t nil)

(autoload 'counsel-clj "counsel" "\
Clojure completion at point.

\(fn)" t nil)

(autoload 'counsel-git "counsel" "\
Find file in the current Git repository.

\(fn)" t nil)

(autoload 'counsel-git-grep "counsel" "\
Grep for a string in the current git repository.
When CMD is a string, use it as a \"git grep\" command.
When CMD is non-nil, prompt for a specific \"git grep\" command.
INITIAL-INPUT can be given as the initial minibuffer input.

\(fn &optional CMD INITIAL-INPUT)" t nil)

(autoload 'counsel-find-file "counsel" "\
Forward to `find-file'.

\(fn)" t nil)

(autoload 'counsel-locate "counsel" "\
Call locate shell command.

\(fn)" t nil)

(autoload 'counsel-load-library "counsel" "\
Load a selected the Emacs Lisp library.
The libraries are offered from `load-path'.

\(fn)" t nil)

(autoload 'counsel-M-x "counsel" "\
Ivy version of `execute-extended-command'.
Optional INITIAL-INPUT is the initial input in the minibuffer.

\(fn &optional INITIAL-INPUT)" t nil)

(autoload 'counsel-load-theme "counsel" "\
Forward to `load-theme'.
Usable with `ivy-resume', `ivy-next-line-and-call' and
`ivy-previous-line-and-call'.

\(fn)" t nil)

(autoload 'counsel-rhythmbox "counsel" "\
Choose a song from the Rhythmbox library to play or enqueue.

\(fn)" t nil)

(autoload 'counsel-org-tag "counsel" "\
Add or remove tags in org-mode.

\(fn)" t nil)

(autoload 'counsel-org-tag-agenda "counsel" "\
Set tags for the current agenda item.

\(fn)" t nil)

(autoload 'counsel-yank-pop "counsel" "\
Ivy replacement for `yank-pop'.

\(fn)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; counsel-autoloads.el ends here
