;;; init-coffee --- initialize coffee-mode
;;; Commentary:
;;; Code:
(require 'use-package)

(use-package coffee-mode
  :defines coffee-mode-map
  :config
  (progn
    (use-package jasmine-coffee)

    (defface hacks/coffee-at-mark
      '((t (:foreground "green")))
      "my at mark")

    (font-lock-add-keywords
     'coffee-mode
     '(("\\(@\\)\\([_[:word:]]+\\|\\<this\\)\\>"
        (1 'hacks/coffee-at-mark)
        (2 font-lock-variable-name-face))))

    (defun coffee-flip-fatness ()
      "Flip fatness of a coffee function arrow."
      (interactive)
      (save-excursion
        (ignore-errors
          (re-search-backward "[-=]>" )
          (when (looking-at "=") (replace-match "-"))
          (when (looking-at "-") (replace-match "=")))))

    (bind-keys :map coffee-mode-map
               ("C-c C-\\" . coffee-flip-fatness)
               ("C-c C-n"  . flymake-goto-next-error)
               ("C-c C-p"  . flymake-goto-prev-error)
               ("C-c C-r"  . coffee-compile-region)
               ("C-c C-,"  . coffee-indent-shift-left)
               ("C-c C-."  . coffee-indent-shift-right))))

(provide 'init-coffee)
;;; init-coffee ends here
