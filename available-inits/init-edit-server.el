;;; init-edit-server -- browser textarea editing delegated to an emacs buffer
;;; Commentary:
;;; Code:
(require 'use-package)

(use-package edit-server
  :init
  (progn (edit-server-start)))

(provide 'init-edit-server)
;;; init-edit-server ends here
