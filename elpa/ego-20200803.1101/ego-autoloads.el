;;; ego-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "ego" "ego.el" (0 0 0 0))
;;; Generated autoloads from ego.el

(autoload 'ego-do-publication "ego" "\
The main entrance of ego. The entire procedure is:
1) verify configuration
2) read changed files on \"org branch\" of \"repository directory\",
   the definition of 'changed files' is:
   1. if FORCE-ALL is non-nil, then all files will be published
      will be published.
   2. if FORCE-ALL is nil, the changed files will be obtained based on the last commit before publish
3) publish org files to html,
   html files will be published on ':store-dir' defined in `ego-project-config-alist'.
4) CHECKIN-ALL checkin all the org-files, with the CHECKIN-ALL you input as the COMMIT STRING.
5) if AUTO-PUSH is non-nil, then EGO push the html and org to the remote repository

\(fn &optional PROJECT-NAME FORCE-ALL AUTO-PUSH CHECKIN-ALL)" t nil)

(autoload 'ego-new-repository "ego" "\
Generate a new git repository in directory REPO-DIR, which can be
perfectly manipulated by EGO. In order to construct a real repository,
you must customize the variable `ego-project-config-alist' according to the readme file of EGO project.

\(fn &optional REPO-DIR ORG-BRANCH STORE-DIR HTML-BRANCH)" t nil)

(autoload 'ego-new-post "ego" "\
Setup a new post.

PROJECT-NAME: which project do you want to export
CATEGORY:     this post belongs to
FILENAME:     the file name of this post

Note that this function does not verify the category and filename, it is users'
responsibility to guarantee the two parameters are valid.

\(fn &optional PROJECT-NAME CATEGORY FILENAME INSERT-FALLBACK-TEMPLATE)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ego" '("ego-")))

;;;***

;;;### (autoloads nil "ego-config" "ego-config.el" (0 0 0 0))
;;; Generated autoloads from ego-config.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ego-config" '("ego-")))

;;;***

;;;### (autoloads nil "ego-export" "ego-export.el" (0 0 0 0))
;;; Generated autoloads from ego-export.el

(autoload 'ego-link-type-process-html "ego-export" "\
Generate EGO-LINK for html export, WARNING: EGO-LINK can only be linked to files in the repository directory

\(fn PATH DESC)" nil nil)

(autoload 'org-ego-link-complete-link "ego-export" "\
Completion function for EGO-LINK. ARG does nothing.

\(fn &optional ARG)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ego-export" '("ego--")))

;;;***

;;;### (autoloads nil "ego-git" "ego-git.el" (0 0 0 0))
;;; Generated autoloads from ego-git.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ego-git" '("ego-")))

;;;***

;;;### (autoloads nil "ego-resource" "ego-resource.el" (0 0 0 0))
;;; Generated autoloads from ego-resource.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ego-resource" '("ego--prepare-theme-resources")))

;;;***

;;;### (autoloads nil "ego-template" "ego-template.el" (0 0 0 0))
;;; Generated autoloads from ego-template.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ego-template" '("ego-")))

;;;***

;;;### (autoloads nil "ego-util" "ego-util.el" (0 0 0 0))
;;; Generated autoloads from ego-util.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ego-util" '("ego-")))

;;;***

;;;### (autoloads nil nil ("ego-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; ego-autoloads.el ends here
