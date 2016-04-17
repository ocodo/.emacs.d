;;; use-git-gutter --- initialize git-gutter
;;; Commentary:
;;; Code:

(if (window-system)
    (progn (require 'git-gutter-fringe+))
  (progn (require 'git-gutter+)))

(add-hook 'prog-mode-hook 'git-gutter+-turn-on)
(add-hook 'css-mode-hook 'git-gutter+-turn-on)

(provide 'use-git-gutter)
;;; use-git-gutter ends here
