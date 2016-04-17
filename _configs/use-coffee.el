;;; use-coffee --- initialize coffee-mode
;;; Commentary:
;;; Code:
(require 'use-package)

(use-package coffee-mode
  :defines coffee-mode-map
  :config
  (progn
    (use-package jasmine-coffee)

    ;; (defface coffee-at-face
    ;;   '((t (:foreground "#FF6600")))
    ;;   "Coffee-Mode face for the @ (this) sigil.")

    ;; (let (at-face)
    ;;   (if (boundp 'coffee-at-face)
    ;;       (setq at-face coffee-at-face)
    ;;     (setq at-face font-lock-string-face))

    ;;   (font-lock-add-keywords
    ;;    'coffee-mode
    ;;    '(("\\(@\\)\\([_[:word:]]+\\|\\<this\\)\\>"
    ;;       (1 at-face)
    ;;       (2 font-lock-variable-name-face)))))

    (bind-keys :map coffee-mode-map
               ("C-c C-\\" . coffee-toggle-fatness)
               ("C-c C-n"  . flymake-goto-next-error)
               ("C-c C-p"  . flymake-goto-prev-error)
               ("C-c C-r"  . coffee-compile-region)
               ("C-c C-,"  . coffee-indent-shift-left)
               ("C-c C-."  . coffee-indent-shift-right))))

(provide 'use-coffee)
;;; use-coffee ends here
