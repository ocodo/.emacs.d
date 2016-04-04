;;; use-edit-server -- browser textarea editing delegated to an emacs buffer
;;; Commentary:
;;; Code:
(require 'use-package)

(use-package edit-server
  :init
  (progn (edit-server-start)))

(provide 'use-edit-server)
;;; use-edit-server ends here
