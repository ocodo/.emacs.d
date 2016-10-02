;;; use-undo-tree --- initialize undo-tree
;;; Commentary:
;;; Code:
(require 'use-package)

(use-package undo-tree
  :config (setq undo-tree-visualizer-timestamps t)
  :bind
  (("C-c C-u" . undo-tree-visualize) 'use-undo-tree))

(provide 'use-undo-tree)
;;; use-undo-tree ends here
