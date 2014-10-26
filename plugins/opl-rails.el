;;; opl-rails --- Rails helpers for the OPL project
;;; Commentary:
;;  Helpful commands for the OPL Rails project
;;; Code:

(require 'projectile)

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
(defvar opl-rails-factory-girl-folder
  "spec/factories"
  "FactoryGirl factories folder.")

;;;###autoload
(defun opl-jump-to-factory-girl-factory ()
  "Jump to a factory girl, factory."
  (interactive)
  (opl-find-file-in-folder opl-rails-factory-girl-folder))

(provide 'opl-rails)
;;; opl-rails.el ends here
