;;; fzf-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "fzf" "fzf.el" (0 0 0 0))
;;; Generated autoloads from fzf.el

(autoload 'fzf "fzf" "\
Starts a fzf session." t nil)

(autoload 'fzf-with-entries "fzf" "\
`entries' is a list of strings that is piped into `fzf' as a
source. `action' is a function that takes a single argument which
is the selected result from `fzf'. `directory' is the directory
to start in.

\(fn ENTRIES ACTION &optional DIRECTORY)" t nil)

(autoload 'fzf-directory "fzf" "\
Starts a fzf session at the specified directory." t nil)

(autoload 'fzf-switch-buffer "fzf" nil t nil)

(autoload 'fzf-find-file "fzf" "\


\(fn &optional DIRECTORY)" t nil)

(autoload 'fzf-find-file-in-dir "fzf" "\


\(fn DIRECTORY)" t nil)

(autoload 'fzf-git-grep "fzf" "\
Starts a fzf session based on git grep result. The input comes
   from the prompt or the selected region." t nil)

(autoload 'fzf-recentf "fzf" nil t nil)

(autoload 'fzf-grep "fzf" "\


\(fn SEARCH &optional DIRECTORY)" t nil)

(autoload 'fzf-git "fzf" "\
Starts an fzf session at the root of the current git project." t nil)

(autoload 'fzf-hg "fzf" "\
Starts an fzf session at the root of the current hg project." t nil)

(autoload 'fzf-git-files "fzf" "\
Starts an fzf session for tracked files in the current git project." t nil)

(autoload 'fzf-projectile "fzf" "\
Starts an fzf session at the root of the current projectile project." t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "fzf" '("fzf")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; fzf-autoloads.el ends here
