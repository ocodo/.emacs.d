;;; init-eww --- initialize eww
;;; Commentary:
;;; Code:
(require 'use-package)

(use-package eww
  :config
  (progn (define-key eww-mode-map (kbd "L") 'browse-web)))

(provide 'init-eww)
;;; init-eww ends here
