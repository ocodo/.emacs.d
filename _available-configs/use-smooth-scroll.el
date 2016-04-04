;; use-smooth-scroll
;; Smoother scrolling (no multiline jumps.)
(setq redisplay-dont-pause t
      scroll-margin 1
      scroll-step 1
      scroll-conservatively 10000
      scroll-preserve-screen-position 1)
(provide 'use-smooth-scroll)
