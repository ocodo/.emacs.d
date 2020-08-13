;;; org-projectile-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "org-projectile" "org-projectile.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from org-projectile.el

(autoload 'org-projectile-goto-location-for-project "org-projectile" "\
Goto the location at which TODOs for PROJECT are stored.

\(fn PROJECT)" t nil)

(autoload 'org-projectile-single-file "org-projectile" "\
Set `org-projectile-strategy' so that captures occur in a single file." t nil)

(autoload 'org-projectile-per-project "org-projectile" "\
Set `org-projectile-strategy' so that captures occur within each project." t nil)

(autoload 'org-projectile-project-todo-completing-read "org-projectile" "\
Select a project using a `projectile-completing-read' and record a TODO.

If CAPTURE-TEMPLATE is provided use it as the capture template
for the TODO. ADDITIONAL-OPTIONS will be supplied as though they
were part of the capture template definition.

\(fn &rest ADDITIONAL-OPTIONS &key CAPTURE-TEMPLATE &allow-other-keys)" t nil)

(autoload 'org-projectile-capture-for-current-project "org-projectile" "\
Capture a TODO for the current active projectile project.

If CAPTURE-TEMPLATE is provided use it as the capture template
for the TODO. ADDITIONAL-OPTIONS will be supplied as though they
were part of the capture template definition.

\(fn &rest ADDITIONAL-OPTIONS &key CAPTURE-TEMPLATE &allow-other-keys)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-projectile" '("occ-" "org-projectile-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; org-projectile-autoloads.el ends here
