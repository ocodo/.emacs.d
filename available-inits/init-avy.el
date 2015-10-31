;;; init-avy --- initialize avy
;;; Commentary:
;;; Code:
(require 'use-package)

(use-package avy
  :init
  (progn (define-key isearch-mode-map (kbd "M-s M-s") 'avy-isearch))
  :bind
  (("M-s M-s w" . avy-goto-word-0)
   ("M-s M-s l" . avy-goto-line)
   ("M-s M-s c" . avy-goto-char)
   ("M-s M-s i" . avy-goto-char-in-line)))

(provide 'init-avy)
;;; init-avy ends here
