;; Customisations for Occur

;; (define-key occur-mode-map (kbd "<up>")
;;   (lambda () (interactive)
;;     (occur-prev)
;;     (occur-mode-goto-occurrence-other-window)
;;     (hl-line-mode 1)
;;     (recenter)
;;     (other-window 1)))

;; (define-key occur-mode-map (kbd "<down>")
;;   (lambda () (interactive)
;;     (occur-next)
;;     (occur-mode-goto-occurrence-other-window)
;;     (hl-line-mode 1)
;;     (recenter)
;;     (other-window 1)))

(provide 'use-occur)
