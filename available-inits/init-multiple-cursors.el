;;; init-multiple-cursors --- initialize multiple cursors
;;; Commentary:
;;; Code:
(require 'use-package)

(use-package multiple-cursors
  :bind
  (("C->" . mc/mark-next-like-this)
   ("C-<" . mc/mark-all-like-this)
   ("C-c C-<" . mc/mark-previous-like-this))

  :init
  (progn
    (define-key cua--rectangle-keymap (kbd "M-U") 'mc/cua-rectangle-to-multiple-cursors)))

(provide 'init-multiple-cursors)
;;; init-multiple-cursors.el ends here
