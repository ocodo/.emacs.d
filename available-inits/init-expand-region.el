;;; init-expand-region --- initialize expand-region
;;; Commentary:
;;; Code:
(require 'use-package)

(use-package expand-region
  :init
  :bind ("C-x x" . er/expand-region))

(provide 'init-expand-region)
;;; init-expand-region.el ends here
