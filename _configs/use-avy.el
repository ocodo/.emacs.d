;;; use-avy --- initialize avy
;;; Commentary:
;;; Code:
(require 'use-package)

(use-package avy
  :init
  (progn (bind-key "M-s M-s" 'avy-isearch isearch-mode-map))
  :config
  (progn
    (use-package dash)

    (defun avy-isearch ()
      "Override to allow avy-background to work as configured."
      (interactive)
      (avy-with avy-isearch
                (avy--process
                 (avy--regex-candidates isearch-string)
                 (avy--style-fn avy-style))
                (isearch-done)))

    (setq avy-background t)
    (setq avy-all-windows 'all-frames)
    (setq avy-style 'de-bruijn)
    (setq avy-keys (-concat (number-sequence ?a ?z) (number-sequence ?A ?Z))))

  :bind (("M-s M-s w" . avy-goto-word-0)
         ("M-s s w"   . avy-goto-word-or-subword-1)
         ("M-s s l"   . avy-goto-line)
         ("M-s s i"   . avy-goto-char-in-line)
         ("M-s s c"   . avy-goto-char)
         ("M-s z t"   . avy-zap-to-char)
         ("M-s z u"   . avy-zap-up-to-char)
         ("M-s c c"   . avy-copy-line)
         ("M-s c m"   . avy-move-line)
         ("M-s c r"   . avy-copy-region)))

(provide 'use-avy)
;;; use-avy ends here
