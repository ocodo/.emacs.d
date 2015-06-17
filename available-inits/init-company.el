;;; Commentary
;;; Code:

(eval-after-load "company-mode"
  (progn
    (global-company-mode 1)
    (company-quickhelp-mode 1)
    (dolist (backend (list 'company-robe
                           'company-elisp
                           'company-yasnippet))
      (add-to-list 'company-backends backend))))

(provide 'init-company)

;;; init-company ends here
