;;; init-avy --- initialize avy
;;; Commentary:
;;; Code:
(require 'use-package)

(use-package avy
  :init
  (progn (bind-keys :map isearch-mode-map ("M-s M-s" . avy-isearch)))
  :config
  (progn
    (setq avy-background t)
    (setq avy-keys (concat (number-sequence ?a ?z)
                       (number-sequence ?A ?Z))))
  :bind
  (("M-s M-s w" . avy-goto-word-0)
   ("M-s M-s l" . avy-goto-line)
   ("M-s M-s c" . avy-goto-char)
   ("M-s M-s i" . avy-goto-char-in-line)
   ("M-s s w"   . avy-goto-word-or-subword-1)
   ("M-s s l"   . avy-goto-line)
   ("M-s s i"   . avy-goto-char-in-line)
   ("M-s s c"   . avy-goto-char)
   ("M-s z t"   . avy-zap-to-char)
   ("M-s z u"   . avy-zap-up-to-char)
   ("M-s c c"   . avy-copy-line)
   ("M-s c m"   . avy-move-line)
   ("M-s c r"   . avy-copy-region)))

(provide 'init-avy)
;;; init-avy ends here
