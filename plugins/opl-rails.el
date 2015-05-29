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

;;;###autoload
(defun opl-rails-find-app-coffee ()
  (interactive)
  (projectile-rails-find-resource
   "coffee app: "
   '(("app/assets/javascripts/opl/app/" "app/assets/javascripts/opl/app/\\(.+\\)\\.js.coffee"))
   "app/assets/javascripts/opl/app/${filename}.js.coffee"))

;;;###autoload
(defun opl-rails-find-data-models-coffee ()
  (interactive)
  (projectile-rails-find-resource
   "data models: "
   '(("app/assets/javascripts/opl/data_models/" "app/assets/javascripts/opl/data_models/\\(.+\\)\\.js.coffee"))
   "app/assets/javascripts/opl/data_models/${filename}.js.coffee"))

;;;###autoload
(defun opl-rails-find-data-store-coffee ()
  (interactive)
  (projectile-rails-find-resource
   "data store: "
   '(("app/assets/javascripts/opl/data_store/" "app/assets/javascripts/opl/data_store/\\(.+\\)\\.js.coffee"))
   "app/assets/javascripts/opl/data_store/${filename}.js.coffee"))

;;;###autoload
(defun opl-rails-find-view-models-coffee ()
  (interactive)
  (projectile-rails-find-resource
   "view models: "
   '(("app/assets/javascripts/opl/view_models/" "app/assets/javascripts/opl/view_models/\\(.+\\)\\.js.coffee"))
   "app/assets/javascripts/opl/view_models/${filename}.js.coffee"))

;;;###autoload
(defun opl-rails-find-helpers-coffee ()
  (interactive)
  (projectile-rails-find-resource
   "coffee helpers: "
   '(("app/assets/javascripts/opl/helpers/" "app/assets/javascripts/opl/helpers/\\(.+\\)\\.js.coffee"))
   "app/assets/javascripts/opl/helpers/${filename}.js.coffee"))

;;;###autoload
(defun opl-rails-find-lib-coffee ()
  (interactive)
  (projectile-rails-find-resource
   "coffee lib: "
   '(("app/assets/javascripts/opl/lib/" "app/assets/javascripts/opl/lib/\\(.+\\)\\.js.coffee")
     ("app/assets/javascripts/lib/" "app/assets/javascripts/lib/\\(.+\\)\\.js.coffee"))
   "app/assets/javascripts/opl/lib/${filename}.js.coffee"))

;;;###autoload
(defun opl-rails-find-models-coffee ()
  (interactive)
  (projectile-rails-find-resource
   "coffee models: "
   '(("app/assets/javascripts/opl/models/" "app/assets/javascripts/opl/models/\\(.+\\)\\.js.coffee"))
   "app/assets/javascripts/opl/models/${filename}.js.coffee"))

;;;###autoload
(defun opl-rails-find-routers-coffee ()
  (interactive)
  (projectile-rails-find-resource
   "coffee routers: "
   '(("app/assets/javascripts/opl/routers/" "app/assets/javascripts/opl/routers/\\(.+\\)\\.js.coffee"))
   "app/assets/javascripts/opl/routers/${filename}.js.coffee"))

;;;###autoload
(defun opl-rails-find-widgets-coffee ()
  (interactive)
  (projectile-rails-find-resource
   "coffee widgets: "
   '(("app/assets/javascripts/opl/widgets/" "app/assets/javascripts/opl/widgets/\\(.+\\)\\.js.coffee"))
   "app/assets/javascripts/opl/widgets/${filename}.js.coffee"))

;;;###autoload
(defun opl-rails-find-views-coffee ()
  (interactive)
  (projectile-rails-find-resource
   "coffee views: "
   '(("app/assets/javascripts/opl/views/" "app/assets/javascripts/opl/views/\\(.+\\)\\.js.coffee"))
   "app/assets/javascripts/opl/views/${filename}.js.coffee"))

;;;###autoload
(defun opl-rails-find-templates-hamlc ()
  (interactive)
  (projectile-rails-find-resource
   "hamlc templates: "
   '(("app/assets/javascripts/opl/templates/" "app/assets/javascripts/opl/templates/\\(.+\\)\\.hamlc"))
   "app/assets/javascripts/opl/templates/${filename}.hamlc"))

;;;###autoload
(defun opl-rails-find-sync-coffee ()
  (interactive)
  (projectile-rails-find-resource
   "coffee sync: "
   '(("app/assets/javascripts/opl/sync/" "app/assets/javascripts/opl/sync/\\(.+\\)\\.js.coffee"))
   "app/assets/javascripts/opl/sync/${filename}.js.coffee"))

;;;###autoload
(defun opl-rails-find-services ()
  (interactive)
  (projectile-rails-find-resource
   "Rails service: "
   '(("app/services/" "app/services/\\(.+\\)\\.rb"))
   "app/services/${filename}.rb"))

;;;###autoload
(defun opl-rails-find-concerns ()
  (interactive)
  (projectile-rails-find-resource
   "Rails service: "
   '(("app/concerns/" "app/concerns/\\(.+\\)\\.rb"))
   "app/concerns/${filename}.rb"))

;;;###autoload
(defun opl-rails-find-workers ()
  (interactive)
  (projectile-rails-find-resource
   "Rails service: "
   '(("app/workers/" "app/workers/\\(.+\\)\\.rb"))
   "app/workers/${filename}.rb"))

;;;###autoload
(defun opl-rails-find-active-admin ()
  (interactive)
  (projectile-rails-find-resource
   "Rails service: "
   '(("app/admin/" "app/admin/\\(.+\\)\\.rb"))
   "app/admin/${filename}.rb"))

;; TODO:
;; - integrate opl-coffee
;; - assist move if in the wrong place / naming convention
;; - generator integration / replacement
;; - more spec helpers / factory / page helpers

(provide 'opl-rails)
;;; opl-rails.el ends here
