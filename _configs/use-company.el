;;; use-company --- configure company-mode
;;; Commentary:
;;; Code:

(use-package company
  :commands (company-mode
             global-company-mode
             company-complete
             company-complete-common
             company-manual-begin
             company-grab-line)
  :config
  (bind-key "C-s" 'company-search-candidates company-mode-map)
  (bind-key "C-M-s" 'company-filter-candidates company-mode-map)

  (setq company-idle-delay 0.01
        company-minimum-prefix-length 2
        company-tooltip-limit 10
        company-dabbrev-downcase nil
        company-dabbrev-ignore-case nil
        company-dabbrev-code-other-buffers t
        company-tooltip-align-annotations t
        company-require-match 'never

        company-frontends '(company-pseudo-tooltip-frontend
                            company-echo-metadata-frontend)

        company-backends '(company-bbdb
                           company-nxml
                           company-css
                           company-eclim
                           company-semantic
                           company-clang
                           company-xcode
                           company-cmake
                           company-capf
                           company-files
                           (company-dabbrev-code
                            company-gtags
                            company-etags
                            company-keywords)
                           company-oddmuse
                           company-dabbrev
                           company-ispell
                           company-capf
                           company-yasnippet)

        company-quickhelp-delay 0.5
        company-statistics-file (concat user-emacs-directory "/company-stats-cache.el"))

  (require 'company-capf)
  (require 'company-yasnippet)

  (push 'company-sort-by-occurrence company-transformers)

  (define-key company-active-map "\C-w" nil)

  (global-company-mode 1)

  ;; NOTE: Doesn't look pretty outside of emacs-mac
  (require 'company-quickhelp)
  (company-quickhelp-mode 1)

  (require 'company-statistics)
  (company-statistics-mode 1))

(use-package company-flx
  :init
  (company-flx-mode 1))

(use-package company-dabbrev
  :commands company-dabbrev)
(use-package company-dabbrev-code
  :commands company-dabbrev-code)
(use-package company-etags
  :commands company-etags)
(use-package company-elisp
  :commands company-elisp)
(use-package company-files
  :commands company-files)
(use-package company-ispell
  :commands company-ispell)
(use-package company-gtags
  :commands company-gtags)

(use-package company-dict
  :commands company-dict
  :ensure t
  :config (setq company-dict-dir (concat user-emacs-directory "/dict")))

(provide 'use-company)
;;; use-company.el ends here
