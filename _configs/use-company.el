;;; use-company --- use company mode
(require 'use-package)

(use-package company

  :init
  (global-company-mode)

  (dolist (backend
           '( ;; list of backends to set up
             company-ispell
             company-elisp
             ))
    (add-to-list 'company-backends
                 backend))

  (setq company-idle-delay 0.1
        company-echo-delay 0.1
        company-minimum-prefix-length 2
        company-show-numbers t
        company-tooltip-align-annotations t))

(provide 'use-company)

;;; use-company.el ends here
