;;; use-expand-region --- initialize expand-region
;;; Commentary:
;;; Code:
(require 'use-package)

(use-package expand-region
  :init
  :bind ("C-x x" . er/expand-region))

(provide 'use-expand-region)
;;; use-expand-region.el ends here
