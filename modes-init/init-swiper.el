;; use Swiper instead of isearch regexp

;; Swiper will override anzu mode for forward isearch / isearch regex
;; We will leave C-r C-M-r set so that macros will work as expected

(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "C-M-s") 'swiper)
