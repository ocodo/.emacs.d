;;; fzf-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "fzf" "fzf.el" (23138 48670 270311 679000))
;;; Generated autoloads from fzf.el

(autoload 'fzf "fzf" "\
Starts a fzf session.

\(fn)" t nil)

(autoload 'fzf-directory "fzf" "\
Starts a fzf session at the specified directory.

\(fn)" t nil)

(autoload 'fzf-git "fzf" "\
Starts a fzf session at the root of the current git.

\(fn)" t nil)

(autoload 'fzf-git-files "fzf" "\
Starts a fzf session only searching for git tracked files.

\(fn)" t nil)

(autoload 'fzf-hg "fzf" "\
Starts a fzf session at the root of the curreng hg.

\(fn)" t nil)

(autoload 'fzf-projectile "fzf" "\
Starts a fzf session at the root of the projectile project.

\(fn)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; fzf-autoloads.el ends here
