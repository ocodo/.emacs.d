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

(defun ego-git-get-all-files (repo-dir &optional branch)
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

(defun ego-git-get-ignored-files (repo-dir)
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
(defun ego-git-get-branch-name (repo-dir)
  "Return name of current branch of git repository presented by REPO-DIR."
  (let* ((repo-dir (file-name-as-directory repo-dir))
        (default-directory repo-dir)
        (branches (vc-git-branches)))
    (car branches)))

(defun ego-git-change-branch (repo-dir branch-name)
  "This function will change branch to BRANCH-NAME of git repository presented
by REPO-DIR only if there is nothing uncommitted in the current branch.

If there is no branch named BRANCH-NAME, It will create an empty brranch"
  (let* ((repo-dir (file-name-as-directory repo-dir))
         (current-branch (ego-git-get-branch-name repo-dir)))
    (if (equal current-branch branch-name)
        (message "current-branch is already %s" branch-name)
      (unless (ego-git-repo-up2date-p repo-dir)
        (error "The branch of %s have something uncommitted, recheck it!" repo-dir))
      (vc-git-checkout nil branch-name))))

(defun ego-git-new-empty-branch (repo-dir branch-name)
  "This function will create a new empty branch with BRANCH-NAME, and checkout it. "
  (let* ((repo-dir (file-name-as-directory repo-dir))
         (default-directory repo-dir)
         (output (ego--shell-command
                  repo-dir
                  (concat "env LC_ALL=C git checkout -b " branch-name)
                  t)))
    (unless (or (string-match "Switched to a new branch" output) (string-match "already exists" output))
      (error "Fatal: Failed to create a new branch with name '%s'."
             branch-name))
    (if (string-match "already exists" output)
        (ego-git-change-branch repo-dir branch-name)
      ;; (ego--shell-command repo-dir "env LC_ALL=C git rm -r ." t)
      (vc-git-command nil 0 nil "rm" "-r" ".")
      (ego-git-commit-changes repo-dir "New empty branch by EGO"))))

(defun ego-git-init-repo (repo-dir)
  "This function will initialize a new empty git repository. REPO-DIR is the
directory where repository will be initialized."
  (unless (file-directory-p repo-dir)
    (mkdir repo-dir t))
  (let ((default-directory repo-dir))
    (vc-git-create-repo)))

(defun ego-git-repo-up2date-p (repo-dir)
  "Judge `REPO-DIR' is up to date or not"
  (let* ((default-directory (file-name-as-directory repo-dir))
         (state (vc-git-state "")))
    (message "repo %s state is %s" repo-dir state)
    (equal state 'up-to-date)))

(defun ego-git-commit-changes (repo-dir commit &optional files)
  "This function will commit uncommitted changes to git repository presented by
REPO-DIR, MESSAGE is the commit message,FILES specify the target files."
  (let* ((repo-dir (file-name-as-directory repo-dir))
         (default-directory repo-dir)
         (files (or files '("."))))
    (unless (ego-git-repo-up2date-p repo-dir)
      (vc-git-register files)
      (vc-git-checkin files commit))))

(defun ego-git-stash-changes (repo-dir name &optional files)
  "This function will create a stash to stash uncommitted changes to git repository presented by
REPO-DIR, NAME is the stash name,FILES specify the target files."
  (let* ((repo-dir (file-name-as-directory repo-dir))
         (default-directory repo-dir)
         (files (or files '("."))))
    (vc-git-register files)
    (vc-git-stash name)))


(defun ego-git-get-changed-files (repo-dir base-commit)
  "This function can get modified/deleted org files from git repository
presented by REPO-DIR, diff based on BASE-COMMIT. The return value is a
property list, property :update maps a list of updated/added files, property
:delete maps a list of deleted files.
For git, there are three types: Added, Modified, Deleted, but for ego,
only two types will work well: need to publish or need to delete.
<TODO>: robust enhance, branch check, etc.未来考虑拆分新增和修改的情况,还有重命名的情况"
  (let ((org-file-ext ".org")
        (repo-dir (file-name-as-directory repo-dir))
        (output (ego--shell-command
                 repo-dir
                 (concat "env LC_ALL=C git diff --name-status "
                         base-commit " HEAD")
                 t))
        upd-list del-list)
    (message "output=%s" output)
    (mapc (lambda (line)
              (let* ((elements (split-string line "\t"))
                     (status (car elements))
                     (rest (cdr elements)))
                (cond ((string-prefix-p "A" status)
                       (push (concat repo-dir (string-join rest "\t")) upd-list))
                      ((string-prefix-p "M" status)
                       (push (concat repo-dir (string-join rest "\t")) upd-list))
                      ((string-prefix-p "D" status)
                       (push (concat repo-dir (string-join rest "\t")) del-list))
                      ((string-prefix-p "R" status) ;TODO 基于假设文件名中不包括TAB
                       (let ((origin-file (car rest))
                             (new-file (cdr rest)))
                         (push (concat repo-dir origin-file) del-list)
                         (push (concat repo-dir (string-join new-file "\t")) upd-list)))
                      )))
          (delete "" (split-string output "\n")))
    (list :update upd-list :delete del-list)))

(defun ego-git-last-change-date (repo-dir filepath)
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

(defun ego-git-first-change-date (repo-dir filepath)
  "This function will return the last commit date of a file in git repository
presented by REPO-DIR, FILEPATH is the path of target file, can be absolute or
relative."
  (let ((repo-dir (file-name-as-directory repo-dir))
        (output (ego--shell-command
                 repo-dir
                 (concat "env LC_ALL=C git log --reverse --format=\"%ci\" -- \"" filepath "\"|head -1")
                 t)))
    (when (string-match "\\`\\([0-9]+-[0-9]+-[0-9]+\\) .*\n\\'" output)
      (match-string 1 output))))

(defun ego-git-remote-name (repo-dir)
  "This function will return all remote repository names of git repository
presented by REPO-DIR, return nil if there is no remote repository."
  (let* ((repo-dir (file-name-as-directory repo-dir))
         (output (ego--shell-command
                  repo-dir
                  "env LC_ALL=C git remote"
                  t)))
    (delete "" (split-string output "\n"))))

(defun ego-git-select-a-remote (repo-dir)
  "Select a remote repository name presented by REPO-DIR,return nil if there is no remote repository"
  (let ((remote-repos (ego-git-remote-name repo-dir)))
    (when remote-repos
      (if (> (length remote-repos) 1)
          (completing-read "Which remote to push?: (if you don't want to push remote, [C-g])"
                           remote-repos nil t)
        (car remote-repos)))))

(defun ego-git-get-publish-config (repo-dir branch)
  "Get publish-config argument for ego--do-publication."
  (let ((remote-repos (ego-git-remote-name repo-dir))
        repo)
    (if (not remote-repos)
        (error "No valid remote repository found.")
      (if (> (length remote-repos) 1)
          (setq repo (completing-read "Which remote to push?: (if you don't want to push remote, [C-g])"
                                      remote-repos nil t))
        (setq repo (car remote-repos)))
      (if (member repo remote-repos)
          (list repo branch)
        (error "Invalid remote repository '%s'." repo)))))

(defun ego-git-find-revision (repo-dir file rev)
  (with-temp-buffer
    (let* (process-file-side-effects
           (coding-system-for-read 'binary)
           (coding-system-for-write 'binary)
           (default-directory repo-dir)
           (fullname
            (let ((fn (vc-git--run-command-string
                       file "ls-files" "-z" "--full-name" "--")))
              ;; ls-files does not return anything when looking for a
              ;; revision of a file that has been renamed or removed.
              (if (string= fn "")
                  (file-relative-name file (vc-git-root default-directory))
                (substring fn 0 -1)))))
      (vc-git-command
       (current-buffer) 0
       nil
       "cat-file" "blob" (concat (if rev rev "HEAD") ":" fullname))
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun ego-git-pull-remote (repo-dir branchs &optional remote-repo)
  "This function will pull remote repository to local branch, REPO-DIR is the
local git repository, REMOTE-REPO is the remote repository, BRANCH is the name
of branch will be pushed."
  (let* ((default-directory (file-name-as-directory repo-dir))
         (remote-repo (or remote-repo
                          (ego-git-select-a-remote repo-dir)))
         (branchs (if (listp branchs)
                      branchs
                    (list branchs)))
         (cmd `("git" "pull" ,remote-repo ,@(mapcar (lambda (branch)
                                                      (concat branch ":" branch))
                                                    branchs)))
         (cmd (string-join cmd " "))
         (output
          (ego--shell-command repo-dir
                              (format "env LC_ALL=C %s" cmd) t)))
    (if (or (string-match "fatal" output)
            (string-match "error" output))
        (error "Failed to pull branch '%s' from remote repository '%s'."
               ,(prin1-to-string branchs) ,remote-repo)
      (with-current-buffer (get-buffer-create ego--temp-buffer-name)
        (goto-char (point-max))
        (insert "remote pull success!")))))


(defun ego-git-push-remote (repo-dir branchs &optional remote-repo)
  "This function will push local branch to remote repository, REPO-DIR is the
local git repository, REMOTE-REPO is the remote repository, BRANCHS is the name(s)
of branch will be pushed (the branch name will be the same both in local and
remote repository), and if there is no branch named BRANCH in remote repository,
it will be created."
  (let* ((default-directory (file-name-as-directory repo-dir))
         (remote-repo (or remote-repo
                          (ego-git-select-a-remote repo-dir)))
         (branchs (if (listp branchs)
                      branchs
                    (list branchs)))
         (cmd `("git" "push" ,remote-repo ,@(mapcar (lambda (branch)
                                                      (concat branch ":" branch))
                                                    branchs)))
         (cmd (string-join cmd " "))
         (output
          (ego--shell-command repo-dir
                              (format "env LC_ALL=C %s" cmd) t)))
    (if (or (string-match "fatal" output)
            (string-match "error" output))
        (error "Failed to push branch '%s' to remote repository '%s'."
               ,(prin1-to-string branchs) ,remote-repo)
      (with-current-buffer (get-buffer-create ego--temp-buffer-name)
        (goto-char (point-max))
        (insert "remote push success!")))))



(provide 'ego-git)

;;; ego-git.el ends here
