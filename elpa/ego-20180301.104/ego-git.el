;;; ego-git.el --- git related functions required by ego

;; Copyright (C)  2015 Feng Shu, Kuangdash
;;                2012, 2013, 2014, 2015 Kelvin Hu

;; Author: Kelvin Hu <ini DOT kelvin AT gmail DOT com>
;;         Feng Shu  <tumashu AT 163.com>
;;         Kuangdash <kuangdash AT 163.com>
;; Keywords: org-mode, convenience, beautify
;; Homepage: https://github.com/emacs-china/EGO
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; git repository operation functions

;;; Code:

(require 'ox)
(require 'ht)
;;(require 'deferred)
(require 'ego-util)
(require 'ego-config)


(defun ego--verify-git-repository (repo-dir)
  "This function will verify whether REPO-DIR is a valid git repository.
TODO: may add branch/commit verification later."
  (unless (and (file-directory-p repo-dir)
               (file-directory-p (expand-file-name ".git/" repo-dir)))
    (error "Fatal: `%s' is not a valid git repository." repo-dir)))

(defun ego--shell-command (dir command &optional need-git)
  "This function execute shell commands in a specified directory.
If NEED-GIT is non-nil, then DIR must be a git repository. COMMAND is the
command to be executed."
  (if need-git
      (ego--verify-git-repository dir))
  (with-current-buffer (get-buffer-create ego--temp-buffer-name)
    (setq default-directory (file-name-as-directory dir))
    (shell-command command t nil)
    (buffer-substring (region-beginning) (region-end))))

(defun ego--git-command (dir args)
  "Execute git command with specified `ARGS' in `DIR'"
  (ego--verify-git-repository dir)
  (let* ((git-args (if (stringp args)
                       args
                     (string-join args " ")))
         (command (concat "git " git-args)))
    ;; (message "%s" command)
    (ego--shell-command dir command t)))

(defun ego--git-all-files (repo-dir &optional branch)
  "This function will return a list contains all org files in git repository
presented by REPO-DIR, if optional BRANCH is offered, will check that branch
instead of pointer HEAD."
  (let ((output (ego--shell-command
                 repo-dir
                 (concat "env LC_ALL=C git ls-tree -r --name-only "
                         (or branch "HEAD"))
                 t)))
    (delq nil (mapcar #'(lambda (line)
                          (when (string-suffix-p ".org" line t)
                            (expand-file-name line repo-dir)))
                      (split-string output "\n")))))

(defun ego--git-ignored-files (repo-dir)
  "This function will return a list of ignored org files in git repository
presented by REPO-DIR."
  (let ((output (ego--shell-command
                 repo-dir
                 (concat "env LC_ALL=C git ls-files --others --ignored --exclude-standard --directory")
                 t)))
    (delq nil (mapcar #'(lambda (line)
                          (when (string-suffix-p ".org" line t)
                            (expand-file-name line repo-dir)))
                      (split-string output "\n")))))

(defun ego--git-branch-name (repo-dir)
  "Return name of current branch of git repository presented by REPO-DIR."
  (let ((repo-dir (file-name-as-directory repo-dir))
        (output (ego--shell-command
                 repo-dir
                 "env LC_ALl=C git rev-parse --abbrev-ref HEAD"
                 t)))
    (replace-regexp-in-string "[\n\r]" "" output)))

(defun ego--git-change-branch (repo-dir branch-name)
  "This function will change branch to BRANCH-NAME of git repository presented
by REPO-DIR only if there is nothing uncommitted in the current branch.

If there is no branch named BRANCH-NAME, It will create an empty brranch"
  (let ((repo-dir (file-name-as-directory repo-dir))
        (output (ego--shell-command
                 repo-dir
                 "env LC_ALL=C git status"
                 t)))
    (when (not (string-match "nothing to commit" output))
      (error "The branch have something uncommitted, recheck it!"))
    (setq output (ego--shell-command
                  repo-dir
                  (concat "env LC_ALL=C git checkout " branch-name)
                  t))
    (when (string-match "\\(\\`error\\|[^a-zA-Z]error\\)" output)
      (if (string-prefix-p (format "error: pathspec '%s' did not match any file(s) known to git." branch-name) output)
          (ego--git-new-empty-branch repo-dir branch-name)
        (error "Failed to change branch to '%s' of repository '%s'."
               branch-name repo-dir)))))

(defun ego--git-new-empty-branch (repo-dir branch-name)
  "This function will create a new empty branch with BRANCH-NAME, and checkout it. "
  (let ((repo-dir (file-name-as-directory repo-dir))
        (output (ego--shell-command
                 repo-dir
                 (concat "env LC_ALL=C git checkout -b " branch-name)
                 t)))
    (unless (or (string-match "Switched to a new branch" output) (string-match "already exists" output))
      (error "Fatal: Failed to create a new branch with name '%s'."
             branch-name))
    (if (string-match "already exists" output)
        (ego--git-change-branch repo-dir branch-name)
      (ego--shell-command repo-dir "env LC_ALL=C git rm -r ." t)
      (ego--shell-command repo-dir "env LC_ALL=C git add .")
      (ego--shell-command repo-dir "env LC_ALL=C git commit -m \"New empty branch by EGO\""))))

(defun ego--git-init-repo (repo-dir)
  "This function will initialize a new empty git repository. REPO-DIR is the
directory where repository will be initialized."
  (unless (file-directory-p repo-dir)
    (mkdir repo-dir t))
  (unless (string-prefix-p "Initialized empty Git repository"
                           (ego--shell-command repo-dir "env LC_ALL=C git init" nil))
    (error "Fatal: Failed to initialize new git repository '%s'." repo-dir)))


(defun ego--git-commit-changes (repo-dir message)
  "This function will commit uncommitted changes to git repository presented by
REPO-DIR, MESSAGE is the commit message."
  (let ((repo-dir (file-name-as-directory repo-dir)) output)
    (ego--shell-command repo-dir "env LC_ALL=C git add ." t)
    (setq output
          (ego--shell-command repo-dir
                              (format "env LC_ALL=C git commit -m \"%s\"" message)
                              t))
    (when (not (or (string-match "\\[.* .*\\]" output) (string-match "nothing to commit" output)))
      (error "Failed to commit changes on current branch of repository '%s'."
             repo-dir))))

(defun ego--git-files-changed (repo-dir base-commit)
  "This function can get modified/deleted org files from git repository
presented by REPO-DIR, diff based on BASE-COMMIT. The return value is a
property list, property :update maps a list of updated/added files, property
:delete maps a list of deleted files.
For git, there are three types: Added, Modified, Deleted, but for ego,
only two types will work well: need to publish or need to delete.
<TODO>: robust enhance, branch check, etc."
  (let ((org-file-ext ".org")
        (repo-dir (file-name-as-directory repo-dir))
        (output (ego--shell-command
                 repo-dir
                 (concat "env LC_ALL=C git diff --name-status "
                         base-commit " HEAD")
                 t))
        upd-list del-list)
    (mapc #'(lambda (line)
              (if (string-match "\\`[A|M]\t\\(.*\.org\\)\\'" line)
                  (setq upd-list (cons (concat repo-dir (match-string 1 line))
                                       upd-list)))
              (if (string-match "\\`D\t\\(.*\.org\\)\\'" line)
                  (setq del-list (cons (concat repo-dir (match-string 1 line))
                                       del-list))))
          (split-string output "\n"))
    (list :update upd-list :delete del-list)))

(defun ego--git-last-change-date (repo-dir filepath)
  "This function will return the last commit date of a file in git repository
presented by REPO-DIR, FILEPATH is the path of target file, can be absolute or
relative."
  (let ((repo-dir (file-name-as-directory repo-dir))
        (output (ego--shell-command
                 repo-dir
                 (concat "env LC_ALL=C git log -1 --format=\"%ci\" -- \"" filepath "\"")
                 t)))
    (when (string-match "\\`\\([0-9]+-[0-9]+-[0-9]+\\) .*\n\\'" output)
      (match-string 1 output))))

(defun ego--git-remote-name (repo-dir)
  "This function will return all remote repository names of git repository
presented by REPO-DIR, return nil if there is no remote repository."
  (let ((repo-dir (file-name-as-directory repo-dir))
        (output (ego--shell-command
                 repo-dir
                 "env LC_ALL=C git remote"
                 t)))
    (delete "" (split-string output "\n"))))

(defun ego--git-get-publish-config (repo-dir org-branch html-branch)
  "Get publish-config argument for ego--do-publication."
  (let ((remote-repos (ego--git-remote-name repo-dir))
        repo branchs)
    (if (not remote-repos)
        (error "No valid remote repository found.")
      (if (> (length remote-repos) 1)
          (setq repo (ido-completing-read "Which remote to push?: (if you don't want to push remote, [C-g])"
                                          remote-repos nil t))
        (setq repo (car remote-repos)))
      (setq branchs
            (ego--ido-completing-read-multiple "(multiple choices)branchs to push: "
                                               (list org-branch html-branch) nil t))
      (if (or (not (member repo remote-repos)) (not branchs))
          (error "Invalid remote repository '%s'." repo)
        (cons repo branchs)))))

(defun ego--git-push-remote (repo-dir remote-repo branchs)
  "This function will push local branch to remote repository, REPO-DIR is the
local git repository, REMOTE-REPO is the remote repository, BRANCH is the name
of branch will be pushed (the branch name will be the same both in local and
remote repository), and if there is no branch named BRANCH in remote repository,
it will be created."
  (let* ((default-directory (file-name-as-directory repo-dir))
         (cmd (append '("git")
                      `("push" ,remote-repo ,@(mapcar (lambda (branch)
                                                        (concat branch ":" branch))
                                                      branchs))))
         (proc (apply #'start-process "EGO-Async" ego--temp-buffer-name cmd)))
    (setq ego--async-publish-success nil)
    (set-process-filter proc `(lambda (proc output)
                                (if (or (string-match "fatal" output)
                                        (string-match "error" output))
                                    (error "Failed to push branch '%s' to remote repository '%s'."
                                           ,(prin1-to-string branchs) ,remote-repo)
                                  (with-current-buffer (get-buffer-create ego--temp-buffer-name)
                                    (setf (point) (point-max))
                                    (insert "remote push success!")
                                    (setq ego--async-publish-success t)))))))

(provide 'ego-git)

;;; ego-git.el ends here
