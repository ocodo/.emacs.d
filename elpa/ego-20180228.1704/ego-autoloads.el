;;; ego-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "ego" "ego.el" (23278 48369 0 0))
;;; Generated autoloads from ego.el

(autoload 'ego-do-publication "ego" "\
The main entrance of ego. The entire procedure is:
1) verify configuration
2) read changed files on \"org branch\" of \"repository directory\",
   the definition of 'changed files' is:
   1. if FORCE-ALL is non-nil, then all files will be published
      will be published.
   2. if FORCE-ALL is nil, the changed files will be obtained based on
      BASE-GIT-COMMIT
   3. if BASE-GIT-COMMIT is nil or omitted, the changed files will be obtained based on previous commit
3) publish org files to html,
   if TEST-AND-NOT-PUBLISH is t, test the generated html files by the web-server,
   otherwise html files will be published on \"html-branch\" of \"repository directory\" and pushed to the remote repository.
4) CHECKIN-ALL checkin all the org-files, with the CHECKIN-ALL you input as the COMMIT STRING.
5) PUBLISH-CONFIG will publish the branchs in the repository, choose remote and corresponding branches. 

\(fn &optional PROJECT-NAME TEST-AND-NOT-PUBLISH FORCE-ALL BASE-GIT-COMMIT CHECKIN-ALL PUBLISH-CONFIG)" t nil)

(autoload 'ego-test-current-page "ego" "\
Test the current opening org-file!

\(fn PROJECT-NAME)" t nil)

(autoload 'ego-new-repository "ego" "\
Generate a new git repository in directory REPO-DIR, which can be
perfectly manipulated by EGO. In order to construct a real repository,
you must customize the variable `ego-project-config-alist' according to the readme file of EGO project.

\(fn REPO-DIR &optional HTML-BRANCH SOURCE-BRANCH)" t nil)

(autoload 'ego-new-post "ego" "\
Setup a new post.

PROJECT-NAME: which project do you want to export
CATEGORY:     this post belongs to
FILENAME:     the file name of this post

Note that this function does not verify the category and filename, it is users'
responsibility to guarantee the two parameters are valid.

\(fn &optional PROJECT-NAME CATEGORY FILENAME INSERT-FALLBACK-TEMPLATE)" t nil)

;;;***

;;;### (autoloads nil "ego-export" "ego-export.el" (23278 48369 0
;;;;;;  0))
;;; Generated autoloads from ego-export.el

(autoload 'ego-link-type-process-html "ego-export" "\
Generate EGO-LINK for html export, WARNING: EGO-LINK can only be linked to files in the repository directory

\(fn PATH DESC)" nil nil)

(autoload 'org-ego-link-complete-link "ego-export" "\
Completion function for EGO-LINK. ARG does nothing.

\(fn &optional ARG)" nil nil)

;;;***

;;;### (autoloads nil "ego-mode" "ego-mode.el" (23278 48369 0 0))
;;; Generated autoloads from ego-mode.el

(autoload 'ego-list-posts "ego-mode" "\
list posts

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("ego-config.el" "ego-devtools.el" "ego-git.el"
;;;;;;  "ego-pkg.el" "ego-resource.el" "ego-template.el" "ego-util.el")
;;;;;;  (23278 48369 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; ego-autoloads.el ends here
