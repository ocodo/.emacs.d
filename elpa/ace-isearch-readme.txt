`ace-isearch.el' provides a minor mode which combines `isearch' and
`ace-jump-mode'.

The "default" behavior can be summrized as:

L = 1     : `ace-jump-mode'
1 < L < 6 : `isearch'
L >= 6    : `helm-swoop-from-isearch'

where L is the input string length during `isearch'.  When L is 1, after a
few seconds specified by `ace-isearch-input-idle-delay', `ace-jump-mode' will
be invoked. Of course you can customize the above behaviour.

Installation:

To use this package, add following code to your init file.

  (require 'ace-isearch)
  (global-ace-isearch-mode +1)
