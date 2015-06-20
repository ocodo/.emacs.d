;;; gitlab-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "gitlab" "gitlab.el" (21893 22976 439401 0))
;;; Generated autoloads from gitlab.el

(autoload 'emacs-gitlab-version "gitlab" "\
Get the emacs-gitlab version as string.
If called interactively or if SHOW-VERSION is non-nil, show the
version in the echo area and the messages buffer.
The returned string includes both, the version from package.el
and the library version, if both a present and different.
If the version number could not be determined, signal an error,
if called interactively, or if SHOW-VERSION is non-nil, otherwise
just return nil.

\(fn &optional SHOW-VERSION)" t nil)

;;;***

;;;### (autoloads nil "gitlab-mode" "gitlab-mode.el" (21893 22976
;;;;;;  443401 0))
;;; Generated autoloads from gitlab-mode.el

(autoload 'gitlab-show-project-description "gitlab-mode" "\
Doc string PROJECT.

\(fn PROJECT)" t nil)

(autoload 'gitlab-show-issues "gitlab-mode" "\
Show Gitlab issues.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "gitlab-session" "gitlab-session.el" (21893
;;;;;;  22976 463401 0))
;;; Generated autoloads from gitlab-session.el

(autoload 'gitlab-login "gitlab-session" "\
Open a session.
If it works, return the private token to perform HTTP request to Gitlab.

\(fn)" nil nil)

;;;***

;;;### (autoloads nil "gitlab-ui" "gitlab-ui.el" (21893 22976 451401
;;;;;;  0))
;;; Generated autoloads from gitlab-ui.el

(autoload 'gitlab-mode "gitlab-ui" "\
Special mode for Gitlab buffers.

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("gitlab-api.el" "gitlab-groups.el" "gitlab-issues.el"
;;;;;;  "gitlab-milestones.el" "gitlab-notes.el" "gitlab-pkg.el"
;;;;;;  "gitlab-projects.el" "gitlab-users.el" "gitlab-utils.el"
;;;;;;  "gitlab-version.el") (21893 22976 481620 710000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; gitlab-autoloads.el ends here
