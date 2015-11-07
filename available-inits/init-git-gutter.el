;;; init-git-gutter --- initialize git-gutter
;;; Commentary:
;;; Code:

(if (window-system)
    (progn (require 'git-gutter-fringe+))
  (progn (require 'git-gutter+)))

(add-hook 'prog-mode-hook 'git-gutter+-turn-on)
(add-hook 'css-mode-hook 'git-gutter+-turn-on)

(provide 'init-git-gutter)
;;; init-git-gutter ends here
