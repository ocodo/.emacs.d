;;; fzf-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "fzf" "fzf.el" (0 0 0 0))
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

(autoload 'fzf-git-grep "fzf" "\
Starts a fzf session based on git grep result. The input comes
   from the prompt or the selected region

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "fzf" '("fzf/")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; fzf-autoloads.el ends here
