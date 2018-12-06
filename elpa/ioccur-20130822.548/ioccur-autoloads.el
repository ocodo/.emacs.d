;;; ioccur-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "ioccur" "ioccur.el" (0 0 0 0))
;;; Generated autoloads from ioccur.el

(autoload 'ioccur-find-buffer-matching "ioccur" "\
Find all buffers containing a text matching REGEXP.
See `ioccur-find-buffer-matching1'.

\(fn REGEXP)" t nil)

(autoload 'ioccur-dired "ioccur" "\


\(fn REGEXP)" t nil)

(autoload 'ioccur-restart "ioccur" "\
Restart `ioccur' from `ioccur-buffer'.
`ioccur-buffer' is erased and a new search is started.

\(fn)" t nil)

(autoload 'ioccur-quit "ioccur" "\
Quit `ioccur-buffer'.

\(fn)" t nil)

(autoload 'ioccur-next-line "ioccur" "\
Goto next line if it is not an empty line.

\(fn)" t nil)

(autoload 'ioccur-precedent-line "ioccur" "\
Goto precedent line if it is not an empty line.

\(fn)" t nil)

(autoload 'ioccur-beginning-of-buffer "ioccur" "\
Goto beginning of `ioccur-buffer'.

\(fn)" t nil)

(autoload 'ioccur-end-of-buffer "ioccur" "\
Go to end of `ioccur-buffer'.

\(fn)" t nil)

(autoload 'ioccur-jump-and-quit "ioccur" "\
Jump to line in other buffer and quit search buffer.

\(fn)" t nil)

(autoload 'ioccur-jump-without-quit "ioccur" "\
Jump to line in `ioccur-current-buffer' without quitting.

\(fn &optional MARK)" t nil)

(autoload 'ioccur-scroll-other-window-down "ioccur" "\
Scroll other window down.

\(fn)" t nil)

(autoload 'ioccur-scroll-other-window-up "ioccur" "\
Scroll other window up.

\(fn)" t nil)

(autoload 'ioccur-scroll-down "ioccur" "\
Scroll down `ioccur-buffer' and `ioccur-current-buffer' simultaneously.

\(fn)" t nil)

(autoload 'ioccur-scroll-up "ioccur" "\
Scroll up `ioccur-buffer' and `ioccur-current-buffer' simultaneously.

\(fn)" t nil)

(autoload 'ioccur-split-window "ioccur" "\
Toggle split window, vertically or horizontally.

\(fn)" t nil)

(autoload 'ioccur "ioccur" "\
Incremental search of lines in current buffer matching input.
With a prefix arg search symbol at point (INITIAL-INPUT).

While you are incremental searching, commands provided are:

C-n or <down>  next line.
C-p or <up>    precedent line.
C-v and M-v    scroll up and down.
C-z or <right> jump without quitting loop.
C-j or <left>  jump and kill `ioccur-buffer'.
RET            exit keeping `ioccur-buffer'.
DEL            remove last character entered.
C-k            Kill current input from cursor to eol.
C-a/e/b/f      Movements in minibuffer.
M-k            Kill current input as sexp.
C-w            Yank stuff at point.
C-g            quit and restore buffer.
C-s            Toggle split window.
C-:            Toggle regexp/litteral search.
C-down         Follow in other buffer.
C-up           Follow in other buffer.
M-p/n          Precedent and next `ioccur-history' element.
M-<, M->       Beginning and end of buffer.

Unlike minibuffer history, cycling in ioccur history have no end:

M-p ,-->A B C D E F G H I---,
    |                       |
    `---I H G F E D C B A<--'

M-n ,-->I H G F E D C B A---,
    |                       |
    `---A B C D E F G H I<--'


Special NOTE for terms:
=======================
  tab/S-tab are bound to history.
  C-d/u are for following in other buffer.
  Use C-t to Scroll up.
 
When you quit incremental search with RET, see `ioccur-mode'
for commands provided in the `ioccur-buffer'.

\(fn &optional INITIAL-INPUT)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ioccur" '("ioccur-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; ioccur-autoloads.el ends here
