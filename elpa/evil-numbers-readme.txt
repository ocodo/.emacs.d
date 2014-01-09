Increment / Decrement binary, octal, decimal and hex literals

works like C-a/C-x in vim, i.e. searches for number up to eol and then
increments or decrements and keep zero padding up

Known Bugs:
See http://github.com/cofi/evil-numbers/issues

Install:

(require 'evil-numbers)

and bind, for example:

(global-set-key (kbd "C-c +") 'evil-numbers/inc-at-pt)
(global-set-key (kbd "C-c -") 'evil-numbers/dec-at-pt)

or only in evil's normal state:

(define-key evil-normal-state-map (kbd "C-c +") 'evil-numbers/inc-at-pt)
(define-key evil-normal-state-map (kbd "C-c -") 'evil-numbers/dec-at-pt)

Usage:
Go and play with your numbers!
