;;; init-eww --- initialize eww
;;; Commentary:
;;; Code:
(require 'use-package)

(use-package eww
  :config
  (progn (bind-key "L" 'browse-web eww-mode-map)))

(provide 'init-eww)
;;; init-eww ends here
