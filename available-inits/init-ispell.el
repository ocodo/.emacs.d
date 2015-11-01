;;; init-ispell --- initialize ispell
;;; Commentary:
;;; Code:
(require 'use-package)

(use-package ispell
  :init
  (progn
    (when (file-exists-p "/usr/local/bin/aspell")
      (set-variable 'ispell-program-name "/usr/local/bin/aspell"))))

(provide 'init-ispell)
;;; init-ispell ends here
