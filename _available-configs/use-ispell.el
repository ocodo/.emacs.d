;;; use-ispell --- initialize ispell
;;; Commentary:
;;; Code:
(require 'use-package)

(use-package ispell
  :init
  (progn
    (when (file-exists-p "/usr/local/bin/aspell")
      (set-variable 'ispell-program-name "/usr/local/bin/aspell"))))

(provide 'use-ispell)
;;; use-ispell ends here
