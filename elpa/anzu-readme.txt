`anzu.el' is Port of `anzu.vim'.

`anzu.el' provides minor-mode which display 'current-posion/total matches'
to mode-line in various search modes. You can understand that how many
does your searched word match in current buffer.

To use this package, add following code to your init.el or .emacs
  (require 'anzu)
  (global-anzu-mode t)
