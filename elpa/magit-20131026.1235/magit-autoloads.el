;;; magit-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (magit-run-gitk magit-run-git-gui-blame magit-run-git-gui
;;;;;;  magit-show magit-show-file-revision magit-init magit-add-change-log-entry-other-window
;;;;;;  magit-add-change-log-entry magit-wazzup magit-save-index
;;;;;;  magit-submodule-sync magit-submodule-init magit-submodule-update-init
;;;;;;  magit-submodule-update magit-commit magit-git-command magit-shell-command
;;;;;;  magit-interactive-rebase magit-rename-remote magit-remove-remote
;;;;;;  magit-add-remote magit-rename-branch magit-delete-branch
;;;;;;  magit-unstage-all magit-stage-all magit-merge-abort magit-merge
;;;;;;  magit-status magit-show-commit) "magit" "magit.el" (21100
;;;;;;  36383 0 0))
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
 (autoload 'magit-checkout "magit")
 (autoload 'magit-create-branch "magit")

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
 (autoload 'magit-reset-head "magit")
 (autoload 'magit-reset-head-hard "magit")
 (autoload 'magit-reset-working-tree "magit")
 (autoload 'magit-fetch "magit")
 (autoload 'magit-fetch-current "magit")
 (autoload 'magit-remote-update "magit")
 (autoload 'magit-pull "magit")

(autoload 'magit-shell-command "magit" "\
Perform arbitrary shell COMMAND.

\(fn COMMAND)" t nil)

(autoload 'magit-git-command "magit" "\
Perform arbitrary Git COMMAND.

Similar to `magit-shell-command', but involves slightly less
typing and automatically refreshes the status buffer.

\(fn COMMAND)" t nil)
 (autoload 'magit-push-tags "magit")
 (autoload 'magit-push "magit")

(autoload 'magit-commit "magit" "\
Create a new commit on HEAD.
With a prefix argument amend to the commit at HEAD instead.
\('git commit [--amend]').

\(fn &optional AMENDP)" t nil)
 (autoload 'magit-tag "magit")
 (autoload 'magit-delete-tag "magit")
 (autoload 'magit-stash "magit")
 (autoload 'magit-stash-snapshot "magit")

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
 (autoload 'magit-log "magit")
 (autoload 'magit-log-ranged "magit")
 (autoload 'magit-log-long "magit")
 (autoload 'magit-log-long-ranged "magit")
 (autoload 'magit-file-log "magit")
 (autoload 'magit-reflog "magit")
 (autoload 'magit-reflog-head "magit")
 (autoload 'magit-cherry "magit")

(autoload 'magit-save-index "magit" "\
Add the content of current file as if it was the index.

\(fn)" t nil)
 (autoload 'magit-diff "magit")
 (autoload 'magit-diff-working-tree "magit")
 (autoload 'magit-diff-working-tree "magit")
 (autoload 'magit-diff-working-tree "magit")

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
 (autoload 'magit-branch-manager "magit")

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
 (autoload 'magit-grep "magit")

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
;;;;;;  magit-bisect-start) "magit-bisect" "magit-bisect.el" (21100
;;;;;;  36383 0 0))
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
;;;;;;  (21100 36383 0 0))
;;; Generated autoloads from magit-blame.el

(autoload 'magit-blame-mode "magit-blame" "\
Display blame information inline.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (global-magit-wip-save-mode magit-wip-save-mode
;;;;;;  magit-wip-mode) "magit-wip" "magit-wip.el" (21100 36383 0
;;;;;;  0))
;;; Generated autoloads from magit-wip.el

(defvar magit-wip-mode nil "\
Non-nil if Magit-Wip mode is enabled.
See the command `magit-wip-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `magit-wip-mode'.")

(custom-autoload 'magit-wip-mode "magit-wip" nil)

(autoload 'magit-wip-mode "magit-wip" "\
In Magit log buffers; give wip refs a special appearance.

\(fn &optional ARG)" t nil)

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

;;;### (autoloads nil nil ("magit-key-mode.el" "magit-pkg.el") (21100
;;;;;;  36383 936925 0))

;;;***

(provide 'magit-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; magit-autoloads.el ends here
