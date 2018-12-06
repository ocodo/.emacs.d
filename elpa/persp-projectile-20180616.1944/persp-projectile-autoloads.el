;;; persp-projectile-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "persp-projectile" "persp-projectile.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from persp-projectile.el

(autoload 'projectile-persp-switch-project "persp-projectile" "\
Switch to a project or perspective we have visited before.
If the perspective of corresponding project does not exist, this
function will call `persp-switch' to create one and switch to
that before `projectile-switch-project' invokes
`projectile-switch-project-action'.

Otherwise, this function calls `persp-switch' to switch to an
existing perspective of the project unless we're already in that
perspective.

\(fn PROJECT-TO-SWITCH)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "persp-projectile" '("projectile-persp-bridge")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; persp-projectile-autoloads.el ends here
