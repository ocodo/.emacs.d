;;; use-eww --- initialize eww
;;; Commentary:
;;; Code:
(require 'use-package)

(use-package eww
  :config
  (progn (bind-key "L" 'browse-web eww-mode-map)))

(provide 'use-eww)
;;; use-eww ends here
