MoveText is extracted from Basic edit toolkit.
It allows you to move the current line using M-up / M-down
if a region is marked, it will move the region instead.


Installation:

Put move-text.el to your load-path.
The load-path is usually ~/elisp/.
It's set in your ~/.emacs like this:
(add-to-list 'load-path (expand-file-name "~/elisp"))

And the following to your ~/.emacs startup file.

(require 'move-text)
(move-text-default-bindings)


Acknowledgements:

 Feature extracted from basid-edit-toolkit.el - by Andy Stewart. (LazyCat)
