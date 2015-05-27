;;; opl-rails --- Rails helpers for the OPL project
;;; Commentary:
;;  Helpful commands for the OPL Rails project
;;; Code:

(require 'projectile)
(require 'projectile-rails)

;;;###autoload
(defun opl-find-file-in-folder (folder)
  "Find file in FOLDER in project."
  (projectile-find-file-in-directory (format "%s" folder)))

;;;###autoload
(defgroup opl-rails nil
  "OPL Rails."
  :group 'ruby)

;;;###autoload
(defcustom opl-rails-siteprism-pages-folder
  "spec/features/support/pages/"
  "SitePrism pages folder."
  :group 'opl-rails
  :type 'string)

;;;###autoload
(defvar opl-rails-siteprism-user-pages-folder
  (format "%suser" opl-rails-siteprism-pages-folder)
  "User pages folder.")

;;;###autoload
(defvar opl-rails-siteprism-admin-pages-folder
  (format "%sadmin" opl-rails-siteprism-pages-folder)
  "Admin pages folder.")

;;; Navigational tools

;; Jump to Pages
;;;###autoload
(defun opl-jump-siteprism-page ()
  "Start fuzzy match at pages root."
  (interactive)
  (opl-find-file-in-folder opl-rails-siteprism-pages-folder))

;;;###autoload
(defun opl-jump-siteprism-user-page ()
  "Start fuzzy match at user pages root."
  (interactive)
  (opl-find-file-in-folder opl-rails-siteprism-user-pages-folder))

;;;###autoload
(defun opl-jump-siteprism-admin-page ()
  "Start fuzzy match at user pages root."
  (interactive)
  (opl-find-file-in-folder opl-rails-siteprism-admin-pages-folder))

;; Jump to Factories
;;;###autoload
(defun projectile-opl-rails-find-view-model ()
  (interactive)
  (projectile-rails-find-resource
   "view-model: "
   '(("app/assets/javascripts/opl/view_models/" "\\(.+\\)\\.js\\.coffee"))))

;;;###autoload
(defun projectile-opl-rails-find-feature ()
  (interactive)
  (projectile-rails-find-resource
   "feature: "
   '(("spec/features/" "spec/features/\\(.+\\)_spec\\.rb$"))
   "spec/features/${filename}.rb"))

;;;###autoload
(defun projectile-opl-rails-find-jasmine ()
  (interactive)
  (projectile-rails-find-resource
   "jasmine: "
   '(("spec/javascripts/" "spec/javascripts/\\(.+\\)_spec\\.js\\.coffee$"))
   "spec/javascripts/${filename}_spec.js.coffee"))

;;;###autoload
(defun projectile-opl-rails-find-siteprism-page ()
  (interactive)
  (projectile-rails-find-resource
   "siteprism page: "
   '(("spec/features/support/pages/" "spec/features/support/pages/\\(.+\\)\\.rb"))
   "spec/features/support/pages/${filename}.rb"))

;;;###autoload
(defun projectile-opl-rails-find-factory ()
  (interactive)
  (projectile-rails-find-resource
   "factory: "
   '(("spec/factories/" "spec/factories/\\(.+\\)\\.rb")) "spec/factories/${filename}.rb"))

;;;###autoload
(defun projectile-opl-rails-find-current-factory ()
  (interactive)
  (beginning-of-line)
  (search-forward-regexp "create\\(_list\\)? :\\([[:alnum:]_]*\\)")
  (let ((factory (pluralize-string (match-string-no-properties 2))))
    (if factory
        (find-file-other-window (format "%s/spec/factories/%s.rb" (projectile-project-root) factory))
      (projectile-opl-rails-find-factory))))

(provide 'opl-rails)
;;; opl-rails.el ends here
