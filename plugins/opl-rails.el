;;; opl-rails --- OPL extension of Projectile Rails
;;; Commentary:
;;  Helpful commands for OPL Rails
;;  see projectile-rails-find-resource for more info,
;;  a VERY useful file finder...
;;; Code:

(require 'projectile)
(require 'projectile-rails)

;;;###autoload
(defun opl-rails-find-view-model ()
  (interactive)
  (projectile-rails-find-resource
   "view-model: "
   '(("app/assets/javascripts/opl/view_models/" "\\(.+\\)\\.js\\.coffee"))))

;;;###autoload
(defun opl-rails-find-feature ()
  (interactive)
  (projectile-rails-find-resource
   "feature: "
   '(("spec/features/" "spec/features/\\(.+\\)_spec\\.rb$"))
   "spec/features/${filename}.rb"))

;;;###autoload
(defun opl-rails-find-jasmine ()
  (interactive)
  (projectile-rails-find-resource
   "jasmine: "
   '(("spec/javascripts/" "spec/javascripts/\\(.+\\)_spec\\.js\\.coffee$"))
   "spec/javascripts/${filename}_spec.js.coffee"))

;;;###autoload
(defun opl-rails-find-siteprism-page ()
  (interactive)
  (projectile-rails-find-resource
   "siteprism page: "
   '(("spec/features/support/pages/" "spec/features/support/pages/\\(.+\\)\\.rb"))
   "spec/features/support/pages/${filename}.rb"))

;;;###autoload
(defun opl-rails-find-factory ()
  (interactive)
  (projectile-rails-find-resource
   "factory: "
   '(("spec/factories/" "spec/factories/\\(.+\\)\\.rb"))
   "spec/factories/${filename}.rb"))

;;;###autoload
(defun opl-rails-find-current-factory ()
  (interactive)
  (beginning-of-line)
  (search-forward-regexp "create\\(_list\\)?(? ?:\\([[:alnum:]_]*\\)")
  (let ((factory (pluralize-string (match-string-no-properties 2))))
    (if factory
        (find-file-other-window (format "%s/spec/factories/%s.rb" (projectile-project-root) factory))
      (opl-rails-find-factory))))

;; TODO:
;; - integrate opl-coffee
;; - use projectile rails find resource pattern
;; - find data store
;; - find concerns
;; - find service
;; - find ... get tree of js/coffee and specs/javascripts
;; - assist move if in the wrong place / naming convention
;; - generator integration / replacement
;; - more spec helpers / factory / page helpers

(provide 'opl-rails)
;;; opl-rails.el ends here
