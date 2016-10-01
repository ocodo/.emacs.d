;;; use-swiper --- Use swiper
;;; Commentary:
;;  use Swiper instead of isearch regexp

;;; Code:
(require 'swiper)

(bind-key "C-S-s" 'swiper)

(provide 'use-swiper)

;;; use-swiper.el ends here
