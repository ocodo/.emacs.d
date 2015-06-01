;;; occur-follow.el --- a simple addition to occur key map to add instant highlighting of occurences in context
;;
;; Author: Jason Milkins <jasonm23@gmail.com>
;; Version: 1.0

;;; Commentary:

;; A simple addition to the occur-mode-map to add instant highlighting
;; of occurences in context.

;;; Code:
(define-key occur-mode-map (kbd "<down>")     ; Define the down arrow key in Occur mode.
  (lambda () (interactive)                    ; We make this an interactive command/function
    (occur-next)                              ; Move to the next occurence...
    (occur-mode-goto-occurrence-other-window) ; Goto the occurence in the file, and move cursor focus to that window.
    (hl-line-mode 1)                          ; Turn on highlight line mode
    (recenter)                                ; Recenter the window around the current occurence line
    (other-window 1)))                        ; Go back to the Occur mode window

(define-key occur-mode-map (kbd "<up>")       ; Define the up arrow key in Occur mode.
  (lambda () (interactive)                    ; We make this an interactive command/function
    (occur-prev)                              ; Move to the previous occurence...
    (occur-mode-goto-occurrence-other-window) ; Goto the occurence in the file, and move cursor focus to that window.
    (hl-line-mode 1)                          ; Turn on highlight line mode
    (recenter)                                ; Recenter the window around the current occurence line
    (other-window 1)))                        ; Go back to the Occur mode window

(provide 'occur-follow)

;;; occur-follow.el ends here
