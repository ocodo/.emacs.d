;;; magit-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (magit-run-gitk magit-run-git-gui-blame magit-run-git-gui
;;;;;;  magit-grep magit-show magit-show-file-revision magit-init
;;;;;;  magit-branch-manager magit-add-change-log-entry-other-window
;;;;;;  magit-add-change-log-entry magit-wazzup magit-diff-unstaged
;;;;;;  magit-diff-staged magit-diff-working-tree magit-diff magit-save-index
;;;;;;  magit-cherry magit-reflog-head magit-reflog magit-file-log
;;;;;;  magit-log-long-ranged magit-log-long magit-log-ranged magit-log
;;;;;;  magit-submodule-sync magit-submodule-init magit-submodule-update-init
;;;;;;  magit-submodule-update magit-stash-snapshot magit-stash magit-delete-tag
;;;;;;  magit-tag magit-commit magit-push magit-push-tags magit-git-command
;;;;;;  magit-shell-command magit-pull magit-remote-update magit-fetch-current
;;;;;;  magit-fetch magit-reset-working-tree magit-reset-head-hard
;;;;;;  magit-reset-head magit-interactive-rebase magit-rename-remote
;;;;;;  magit-remove-remote magit-add-remote magit-rename-branch
;;;;;;  magit-delete-branch magit-create-branch magit-checkout magit-unstage-all
;;;;;;  magit-stage-all magit-merge-abort magit-merge magit-status
;;;;;;  magit-show-commit) "magit" "magit.el" (21110 65087 0 0))
;;; Generated autoloads from magit.el

(autoload 'magit-show-commit "magit" "\
Show information about a commit.
Show it in the buffer named by `magit-commit-buffer-name'.
COMMIT can be any valid name for a commit in the current Git
repository.

When called interactively or when SELECT is non-nil, switch to
the commit buffer using `pop-to-buffer'.

Unless INHIBIT-HISTORY is non-nil, the commit currently shown
will be pushed onto `magit-back-navigation-history' and
`magit-forward-navigation-history' will be cleared.

Noninteractively, if the commit is already displayed and SCROLL
is provided, call SCROLL's function definition in the commit
window.  (`scroll-up' and `scroll-down' are typically passed in
for this argument.)

\(fn COMMIT &optional SCROLL INHIBIT-HISTORY SELECT)" t nil)

(autoload 'magit-status "magit" "\
Open a Magit status buffer for the Git repository containing DIR.
If DIR is not within a Git repository, offer to create a Git
repository in DIR.

Interactively, a prefix argument means to ask the user which Git
repository to use even if `default-directory' is under Git
control.  Two prefix arguments means to ignore `magit-repo-dirs'
when asking for user input.

\(fn DIR)" t nil)

(autoload 'magit-merge "magit" "\
Merge REVISION into the current 'HEAD', leaving changes uncommitted.
With a prefix argument, skip editing the log message and commit.
\('git merge [--no-commit] REVISION').

\(fn REVISION &optional DO-COMMIT)" t nil)

(autoload 'magit-merge-abort "magit" "\
Abort the current merge operation.

\(fn)" t nil)

(autoload 'magit-stage-all "magit" "\
Add all remaining changes in tracked files to staging area.
With a prefix argument, add remaining untracked files as well.
\('git add [-u] .').

\(fn &optional INCLUDING-UNTRACKED)" t nil)

(autoload 'magit-unstage-all "magit" "\
Remove all changes from staging area.
\('git reset --mixed HEAD').

\(fn)" t nil)

(autoload 'magit-checkout "magit" "\
Switch 'HEAD' to REVISION and update working tree.
Fails if working tree or staging area contain uncommitted changes.
If REVISION is a remote branch, offer to create a local tracking branch.
\('git checkout [-b] REVISION').

\(fn REVISION)" t nil)

(autoload 'magit-create-branch "magit" "\
Switch 'HEAD' to new BRANCH at revision PARENT and update working tree.
Fails if working tree or staging area contain uncommitted changes.
\('git checkout -b BRANCH REVISION').

\(fn BRANCH PARENT)" t nil)

(autoload 'magit-delete-branch "magit" "\
Delete the BRANCH.
If the branch is the current one, offers to switch to `master' first.
With prefix, forces the removal even if it hasn't been merged.
Works with local or remote branches.
\('git branch [-d|-D] BRANCH' or 'git push <remote-part-of-BRANCH> :refs/heads/BRANCH').

\(fn BRANCH &optional FORCE)" t nil)

(autoload 'magit-rename-branch "magit" "\
Rename branch OLD to NEW.
With prefix, forces the rename even if NEW already exists.
\('git branch [-m|-M] OLD NEW').

\(fn OLD NEW &optional FORCE)" t nil)

(autoload 'magit-add-remote "magit" "\
Add the REMOTE and fetch it.
\('git remote add REMOTE URL').

\(fn REMOTE URL)" t nil)

(autoload 'magit-remove-remote "magit" "\
Delete the REMOTE.
\('git remote rm REMOTE').

\(fn REMOTE)" t nil)

(autoload 'magit-rename-remote "magit" "\
Rename remote OLD to NEW.
\('git remote rename OLD NEW').

\(fn OLD NEW)" t nil)

(autoload 'magit-interactive-rebase "magit" "\
Start a git rebase -i session, old school-style.

\(fn COMMIT)" t nil)

(autoload 'magit-reset-head "magit" "\
Switch 'HEAD' to REVISION, keeping prior working tree and staging area.
Any differences from REVISION become new changes to be committed.
With prefix argument, all uncommitted changes in working tree
and staging area are lost.
\('git reset [--soft|--hard] REVISION').

\(fn REVISION &optional HARD)" t nil)

(autoload 'magit-reset-head-hard "magit" "\
Switch 'HEAD' to REVISION, losing all changes.
Uncomitted changes in both working tree and staging area are lost.
\('git reset --hard REVISION').

\(fn REVISION)" t nil)

(autoload 'magit-reset-working-tree "magit" "\
Revert working tree and clear changes from staging area.
\('git reset --hard HEAD').

With a prefix arg, also remove untracked files.
With two prefix args, remove ignored files as well.

\(fn &optional ARG)" t nil)

(autoload 'magit-fetch "magit" "\
Fetch from REMOTE.

\(fn REMOTE)" t nil)

(autoload 'magit-fetch-current "magit" "\
Run fetch for default remote.

If there is no default remote, ask for one.

\(fn)" t nil)

(autoload 'magit-remote-update "magit" "\
Update all remotes.

\(fn)" t nil)

(autoload 'magit-pull "magit" "\
Run git pull.

If there is no default remote, the user is prompted for one and
its values is saved with git config.  If there is no default
merge branch, the user is prompted for one and its values is
saved with git config.  With a prefix argument, the default
remote is not used and the user is prompted for a remote.  With
two prefix arguments, the default merge branch is not used and
the user is prompted for a merge branch.  Values entered by the
user because of prefix arguments are not saved with git config.

\(fn)" t nil)

(autoload 'magit-shell-command "magit" "\
Perform arbitrary shell COMMAND.

\(fn COMMAND)" t nil)

(autoload 'magit-git-command "magit" "\
Perform arbitrary Git COMMAND.

Similar to `magit-shell-command', but involves slightly less
typing and automatically refreshes the status buffer.

\(fn COMMAND)" t nil)

(autoload 'magit-push-tags "magit" "\
Push tags to a remote repository.

Push tags to the current branch's remote.  If that isn't set push
to \"origin\" or if that remote doesn't exit but only a single
remote is defined use that.  Otherwise or with a prefix argument
ask the user what remote to use.

\(fn)" t nil)

(autoload 'magit-push "magit" "\
Push the current branch to a remote repository.

By default push to the remote specified by the git-config(1) option
branch.<name>.remote or else origin.  Otherwise or with a prefix
argument instead ask the user what remote to push to.

When pushing to branch.<name>.remote push to the branch specified by
branch.<name>.merge.  When pushing to another remote or if the latter
option is not set push to the remote branch with the same name as the
local branch being pushed.  With two or more prefix arguments instead
ask the user what branch to push to.  In this last case actually push
even if `magit-set-upstream-on-push's value is `refuse'.

\(fn)" t nil)

(autoload 'magit-commit "magit" "\
Create a new commit on HEAD.
With a prefix argument amend to the commit at HEAD instead.
\('git commit [--amend]').

\(fn &optional AMENDP)" t nil)

(autoload 'magit-tag "magit" "\
Create a new tag with the given NAME at REV.
With a prefix argument annotate the tag.
\('git tag [--annotate] NAME REV').

\(fn NAME REV &optional ANNOTATE)" t nil)

(autoload 'magit-delete-tag "magit" "\
Delete the tag with the given NAME.
\('git tag -d NAME').

\(fn NAME)" t nil)

(autoload 'magit-stash "magit" "\
Create new stash of working tree and staging area named DESCRIPTION.
Working tree and staging area revert to the current 'HEAD'.
With prefix argument, changes in staging area are kept.
\('git stash save [--keep-index] DESCRIPTION')

\(fn DESCRIPTION)" t nil)

(autoload 'magit-stash-snapshot "magit" "\
Create new stash of working tree and staging area; keep changes in place.
\('git stash save \"Snapshot...\"; git stash apply stash@{0}')

\(fn)" t nil)

(autoload 'magit-submodule-update "magit" "\
Update the submodule of the current git repository.
With a prefix arg, do a submodule update --init.

\(fn &optional INIT)" t nil)

(autoload 'magit-submodule-update-init "magit" "\
Update and init the submodule of the current git repository.

\(fn)" t nil)

(autoload 'magit-submodule-init "magit" "\
Initialize the submodules.

\(fn)" t nil)

(autoload 'magit-submodule-sync "magit" "\
Synchronizes submodule's remote URL configuration.

\(fn)" t nil)

(autoload 'magit-log "magit" "\


\(fn &optional RANGE)" t nil)

(autoload 'magit-log-ranged "magit" "\


\(fn RANGE)" t nil)

(autoload 'magit-log-long "magit" "\


\(fn &optional RANGE)" t nil)

(autoload 'magit-log-long-ranged "magit" "\


\(fn RANGE)" t nil)

(autoload 'magit-file-log "magit" "\
Display the log for the currently visited file or another one.
With a prefix argument show the log graph.

\(fn FILE &optional USE-GRAPH)" t nil)

(autoload 'magit-reflog "magit" "\


\(fn REF)" t nil)

(autoload 'magit-reflog-head "magit" "\


\(fn)" t nil)

(autoload 'magit-cherry "magit" "\


\(fn HEAD UPSTREAM)" t nil)

(autoload 'magit-save-index "magit" "\
Add the content of current file as if it was the index.

\(fn)" t nil)

(autoload 'magit-diff "magit" "\


\(fn RANGE &optional WORKING ARGS)" t nil)

(autoload 'magit-diff-working-tree "magit" "\


\(fn REV)" t nil)

(autoload 'magit-diff-staged "magit" "\
Show differences between index and HEAD.

\(fn)" t nil)

(autoload 'magit-diff-unstaged "magit" "\
Show differences between working tree and index.

\(fn)" t nil)

(autoload 'magit-wazzup "magit" "\


\(fn BRANCH)" t nil)

(autoload 'magit-add-change-log-entry "magit" "\
Find change log file and add date entry and item for current change.
This differs from `add-change-log-entry' (which see) in that
it acts on the current hunk in a Magit buffer instead of on
a position in a file-visiting buffer.

\(fn &optional WHOAMI FILE-NAME OTHER-WINDOW)" t nil)

(autoload 'magit-add-change-log-entry-other-window "magit" "\


\(fn &optional WHOAMI FILE-NAME)" t nil)

(autoload 'magit-branch-manager "magit" "\


\(fn)" t nil)

(autoload 'magit-init "magit" "\
Initialize git repository in the DIR directory.

\(fn DIR)" t nil)

(autoload 'magit-show-file-revision "magit" "\
Open a new buffer showing the current file in the revision at point.

\(fn)" t nil)

(autoload 'magit-show "magit" "\
Return a buffer containing the file FILENAME, as stored in COMMIT.

COMMIT may be one of the following:
- A string with the name of a commit, such as \"HEAD\" or
  \"dae86e\".  See 'git help revisions' for syntax.
- The symbol 'index, indicating that you want the version in
  Git's index or staging area.
- The symbol 'working, indicating that you want the version in
  the working directory.  In this case you'll get a buffer
  visiting the file.  If there's already a buffer visiting that
  file, you'll get that one.

When called interactively or when SELECT is non-nil, make the
buffer active, either in another window or (with a prefix
argument) in the current window.

\(fn COMMIT FILENAME &optional SELECT PREFIX)" t nil)

(autoload 'magit-grep "magit" "\


\(fn PATTERN)" t nil)

(autoload 'magit-run-git-gui "magit" "\
Run `git gui' for the current git repository.

\(fn)" t nil)

(autoload 'magit-run-git-gui-blame "magit" "\
Run `git gui blame' on the given FILENAME and COMMIT.
Interactively run it for the current file and the HEAD, with a
prefix let the user choose.  When the current buffer is visiting
FILENAME instruct blame to center around the line point is on.

\(fn COMMIT FILENAME &optional LINENUM)" t nil)

(autoload 'magit-run-gitk "magit" "\
Run `gitk --all' for the current git repository.

\(fn)" t nil)

;;;***

;;;### (autoloads (magit-bisect-run magit-bisect-visualize magit-bisect-log
;;;;;;  magit-bisect-skip magit-bisect-bad magit-bisect-good magit-bisect-reset
;;;;;;  magit-bisect-start) "magit-bisect" "magit-bisect.el" (21110
;;;;;;  65087 0 0))
;;; Generated autoloads from magit-bisect.el

(autoload 'magit-bisect-start "magit-bisect" "\
Start a bisect session.

\(fn)" t nil)

(autoload 'magit-bisect-reset "magit-bisect" "\
Quit a bisect session.

\(fn)" t nil)

(autoload 'magit-bisect-good "magit-bisect" "\
Tell git that the current revision is good during a bisect session.

\(fn)" t nil)

(autoload 'magit-bisect-bad "magit-bisect" "\
Tell git that the current revision is bad during a bisect session.

\(fn)" t nil)

(autoload 'magit-bisect-skip "magit-bisect" "\
Tell git to skip the current revision during a bisect session.

\(fn)" t nil)

(autoload 'magit-bisect-log "magit-bisect" "\
Show the bisect log.

\(fn)" t nil)

(autoload 'magit-bisect-visualize "magit-bisect" "\
Show the remaining suspects with gitk.

\(fn)" t nil)

(autoload 'magit-bisect-run "magit-bisect" "\
Bisect automatically by running commands after each step.

\(fn COMMAND)" t nil)

;;;***

;;;### (autoloads (magit-blame-mode) "magit-blame" "magit-blame.el"
;;;;;;  (21110 65087 0 0))
;;; Generated autoloads from magit-blame.el

(autoload 'magit-blame-mode "magit-blame" "\
Display blame information inline.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (global-magit-wip-save-mode magit-wip-save-mode)
;;;;;;  "magit-wip" "magit-wip.el" (21110 65087 0 0))
;;; Generated autoloads from magit-wip.el

(autoload 'magit-wip-save-mode "magit-wip" "\
Magit support for committing to a work-in-progress ref.

When this minor mode is turned on and a file is saved inside a
writable git repository then it is also committed to a special
work-in-progress ref.

\(fn &optional ARG)" t nil)

(defvar global-magit-wip-save-mode nil "\
Non-nil if Global-Magit-Wip-Save mode is enabled.
See the command `global-magit-wip-save-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-magit-wip-save-mode'.")

(custom-autoload 'global-magit-wip-save-mode "magit-wip" nil)

(autoload 'global-magit-wip-save-mode "magit-wip" "\
Toggle Magit-Wip-Save mode in all buffers.
With prefix ARG, enable Global-Magit-Wip-Save mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Magit-Wip-Save mode is enabled in all buffers where
`turn-on-magit-wip-save' would do it.
See `magit-wip-save-mode' for more information on Magit-Wip-Save mode.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil nil ("magit-key-mode.el" "magit-pkg.el") (21110
;;;;;;  65087 368203 0))

;;;***

(provide 'magit-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; magit-autoloads.el ends here
