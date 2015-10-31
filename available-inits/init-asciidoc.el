;;; init-asciidoc --- init asciidoc
;;; Commentary:
;;; Code:
(require 'use-package)

(use-package asciidoc-mode
  :defer t
  :init (add-to-list 'auto-mode-alist '("\\.asciidoc$" . asciidoc-mode)))

(provide 'init-asciidoc)
;;; init-asciidoc ends here
