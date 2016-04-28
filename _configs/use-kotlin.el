;;; init-kotlin --- initialize kotlin
;;; Commentary:
;;; Code:
(require 'use-package)

(use-package kotlin-mode
  :init ;; before use
    (add-to-list 'auto-mode-alist '("\\.kt\\'" . kotlin-mode)))

(provide 'init-kotlin)
;;; init-kotlin ends here
