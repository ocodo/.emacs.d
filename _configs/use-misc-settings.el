
;; use-misc-settings
(show-paren-mode 1)
(setq show-paren-delay 0.25)
(setq kill-whole-line 1)
(fset 'yes-or-no-p 'y-or-n-p)

(put 'set-goal-column           'disabled nil)
(put 'erase-buffer              'disabled nil)
(put 'downcase-region           'disabled nil)
(put 'upcase-region             'disabled nil)
(put 'narrow-to-region          'disabled nil)
(put 'narrow-to-page            'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
(put 'scroll-left               'disabled nil)

;; Turn Linum on for a buffer with `super-\' when needed.
;; note line number is on modeline.
;;
;; Linum is deathly slow, so it's always best to avoid unless really
;; necessary.
(global-linum-mode -1)

(provide 'use-misc-settings)
