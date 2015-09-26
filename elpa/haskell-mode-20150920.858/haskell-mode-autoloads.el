;;; haskell-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "ghc-core" "ghc-core.el" (22021 61696 13140
;;;;;;  0))
;;; Generated autoloads from ghc-core.el

(let ((loads (get 'ghc-core 'custom-loads))) (if (member '"ghc-core" loads) nil (put 'ghc-core 'custom-loads (cons '"ghc-core" loads))))

(defvar ghc-core-program "ghc" "\
Name of the GHC executable (excluding any arguments).")

(custom-autoload 'ghc-core-program "ghc-core" t)

(defvar ghc-core-program-args '("-O2") "\
Additional options to be passed to GHC when generating core output.
GHC (see variable `ghc-core-program') is invoked with the basic
command line options \"-ddump-simpl -c <source-file>\"
followed by the additional options defined here.

The following `-ddump-simpl` options might be of interest:

 - `-dsuppress-all'
 - `-dsuppress-uniques'
 - `-dsuppress-idinfo'
 - `-dsuppress-module-prefixes'
 - `-dsuppress-type-signatures'
 - `-dsuppress-type-applications'
 - `-dsuppress-coercions'

See `M-x manual-entry RET ghc' for more details.")

(custom-autoload 'ghc-core-program-args "ghc-core" t)

(autoload 'ghc-core-create-core "ghc-core" "\
Compile and load the current buffer as tidy core.

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.hcr\\'" . ghc-core-mode))

(add-to-list 'auto-mode-alist '("\\.dump-simpl\\'" . ghc-core-mode))

(autoload 'ghc-core-mode "ghc-core" "\
Major mode for GHC Core files.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "ghci-script-mode" "ghci-script-mode.el" (22021
;;;;;;  61695 965140 0))
;;; Generated autoloads from ghci-script-mode.el

(autoload 'ghci-script-mode "ghci-script-mode" "\
Major mode for working with .ghci files.

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.ghci\\'" . ghci-script-mode))

;;;***

;;;### (autoloads nil "haskell" "haskell.el" (22021 61696 29140 0))
;;; Generated autoloads from haskell.el

(autoload 'interactive-haskell-mode "haskell" "\
Minor mode for enabling haskell-process interaction.

\(fn &optional ARG)" t nil)

(autoload 'haskell-interactive-mode-return "haskell" "\
Handle the return key.

\(fn)" t nil)

(autoload 'haskell-session-kill "haskell" "\
Kill the session process and buffer, delete the session.
0. Prompt to kill all associated buffers.
1. Kill the process.
2. Kill the interactive buffer.
3. Walk through all the related buffers and set their haskell-session to nil.
4. Remove the session from the sessions list.

\(fn &optional LEAVE-INTERACTIVE-BUFFER)" t nil)

(autoload 'haskell-interactive-kill "haskell" "\
Kill the buffer and (maybe) the session.

\(fn)" t nil)

(autoload 'haskell-session "haskell" "\
Get the Haskell session, prompt if there isn't one or fail.

\(fn)" nil nil)

(autoload 'haskell-interactive-switch "haskell" "\
Switch to the interactive mode for this session.

\(fn)" t nil)

(autoload 'haskell-session-change "haskell" "\
Change the session for the current buffer.

\(fn)" t nil)

(autoload 'haskell-kill-session-process "haskell" "\
Kill the process.

\(fn &optional SESSION)" t nil)

(autoload 'haskell-interactive-mode-visit-error "haskell" "\
Visit the buffer of the current (or last) error message.

\(fn)" t nil)

(autoload 'haskell-mode-contextual-space "haskell" "\
Contextually do clever stuff when hitting space.

\(fn)" t nil)

(autoload 'haskell-mode-jump-to-tag "haskell" "\
Jump to the tag of the given identifier.

\(fn &optional NEXT-P)" t nil)

(autoload 'haskell-mode-after-save-handler "haskell" "\
Function that will be called after buffer's saving.

\(fn)" nil nil)

(autoload 'haskell-mode-tag-find "haskell" "\
The tag find function, specific for the particular session.

\(fn &optional NEXT-P)" t nil)

(autoload 'haskell-interactive-bring "haskell" "\
Bring up the interactive mode for this session.

\(fn)" t nil)

(autoload 'haskell-process-load-file "haskell" "\
Load the current buffer file.

\(fn)" t nil)

(autoload 'haskell-process-reload-file "haskell" "\
Re-load the current buffer file.

\(fn)" t nil)

(autoload 'haskell-process-load-or-reload "haskell" "\
Load or reload. Universal argument toggles which.

\(fn &optional TOGGLE)" t nil)

(autoload 'haskell-process-cabal-build "haskell" "\
Build the Cabal project.

\(fn)" t nil)

(autoload 'haskell-process-cabal "haskell" "\
Prompts for a Cabal command to run.

\(fn P)" t nil)

(autoload 'haskell-process-minimal-imports "haskell" "\
Dump minimal imports.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "haskell-align-imports" "haskell-align-imports.el"
;;;;;;  (22021 61696 117140 0))
;;; Generated autoloads from haskell-align-imports.el

(defvar haskell-align-imports-pad-after-name nil "\
Pad layout after the module name also.")

(custom-autoload 'haskell-align-imports-pad-after-name "haskell-align-imports" t)

(autoload 'haskell-align-imports "haskell-align-imports" "\
Align all the imports in the buffer.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "haskell-bot" "haskell-bot.el" (22021 61696
;;;;;;  113140 0))
;;; Generated autoloads from haskell-bot.el

(let ((loads (get 'haskell-bot 'custom-loads))) (if (member '"haskell-bot" loads) nil (put 'haskell-bot 'custom-loads (cons '"haskell-bot" loads))))

(defvar haskell-bot-program-name "lambdabot" "\
*The name of the Bot interpreter program.")

(custom-autoload 'haskell-bot-program-name "haskell-bot" t)

(defvar haskell-bot-program-args nil "\
*A list of string args to pass when starting the Bot interpreter.")

(custom-autoload 'haskell-bot-program-args "haskell-bot" t)

;;;***

;;;### (autoloads nil "haskell-cabal" "haskell-cabal.el" (22021 61696
;;;;;;  81140 0))
;;; Generated autoloads from haskell-cabal.el

(add-to-list 'auto-mode-alist '("\\.cabal\\'" . haskell-cabal-mode))

(autoload 'haskell-cabal-mode "haskell-cabal" "\
Major mode for Cabal package description files.

\(fn)" t nil)

(autoload 'haskell-cabal-guess-setting "haskell-cabal" "\
Guess the specified setting of this project.
If there is no valid .cabal file to get the setting from (or
there is no corresponding setting with that name in the .cabal
file), then this function returns nil.

\(fn NAME)" t nil)

(autoload 'haskell-cabal-get-dir "haskell-cabal" "\
Get the Cabal dir for a new project. Various ways of figuring this out,
   and indeed just prompting the user. Do them all.

\(fn &optional USE-DEFAULTS)" nil nil)

(autoload 'haskell-cabal-visit-file "haskell-cabal" "\
Locate and visit package description file for file visited by current buffer.
This uses `haskell-cabal-find-file' to locate the closest
\".cabal\" file and open it.  This command assumes a common Cabal
project structure where the \".cabal\" file is in the top-folder
of the project, and all files related to the project are in or
below the top-folder.  If called with non-nil prefix argument
OTHER-WINDOW use `find-file-other-window'.

\(fn OTHER-WINDOW)" t nil)

(let ((loads (get 'haskell-cabal 'custom-loads))) (if (member '"haskell-cabal" loads) nil (put 'haskell-cabal 'custom-loads (cons '"haskell-cabal" loads))))

(defvar haskell-cabal-list-comma-position 'before "\
Where to put the comma in lists")

(custom-autoload 'haskell-cabal-list-comma-position "haskell-cabal" t)

;;;***

;;;### (autoloads nil "haskell-checkers" "haskell-checkers.el" (22021
;;;;;;  61696 65140 0))
;;; Generated autoloads from haskell-checkers.el

(let ((loads (get 'haskell-checkers 'custom-loads))) (if (member '"haskell-checkers" loads) nil (put 'haskell-checkers 'custom-loads (cons '"haskell-checkers" loads))))

(defvar haskell-lint-command "hlint" "\
The default lint command for \\[hlint].")

(custom-autoload 'haskell-lint-command "haskell-checkers" t)

(defvar haskell-scan-command "scan" "\
The default scan command for \\[haskell-scan].")

(custom-autoload 'haskell-scan-command "haskell-checkers" t)

(defvar haskell-scan-options "" "\
The default options for \\[haskell-scan].")

(custom-autoload 'haskell-scan-options "haskell-checkers" t)

(defvar haskell-lint-options "" "\
The default options for \\[hlint].")

(custom-autoload 'haskell-lint-options "haskell-checkers" t)

(defvar haskell-checkers-save-files t "\
Save modified files when run checker or not (ask user)")

(custom-autoload 'haskell-checkers-save-files "haskell-checkers" t)

(defvar haskell-checkers-replace-with-suggestions nil "\
Replace user's code with suggested replacements (hlint only)")

(custom-autoload 'haskell-checkers-replace-with-suggestions "haskell-checkers" t)

(defvar haskell-checkers-replace-without-ask nil "\
Replace user's code with suggested replacements automatically (hlint only)")

(custom-autoload 'haskell-checkers-replace-without-ask "haskell-checkers" t)

;;;***

;;;### (autoloads nil "haskell-commands" "haskell-commands.el" (22021
;;;;;;  61696 109140 0))
;;; Generated autoloads from haskell-commands.el

(autoload 'haskell-process-restart "haskell-commands" "\
Restart the inferior Haskell process.

\(fn)" t nil)

(autoload 'haskell-process-clear "haskell-commands" "\
Clear the current process.

\(fn)" t nil)

(autoload 'haskell-process-interrupt "haskell-commands" "\
Interrupt the process (SIGINT).

\(fn)" t nil)

(autoload 'haskell-describe "haskell-commands" "\
Describe the given identifier IDENT.

\(fn IDENT)" t nil)

(autoload 'haskell-rgrep "haskell-commands" "\
Grep the effective project for the symbol at point.
Very useful for codebase navigation.

Prompts for an arbitrary regexp given a prefix arg PROMPT.

\(fn &optional PROMPT)" t nil)

(autoload 'haskell-process-do-info "haskell-commands" "\
Print info on the identifier at point.
If PROMPT-VALUE is non-nil, request identifier via mini-buffer.

\(fn &optional PROMPT-VALUE)" t nil)

(autoload 'haskell-process-do-type "haskell-commands" "\
Print the type of the given expression.

Given INSERT-VALUE prefix indicates that result type signature
should be inserted.

\(fn &optional INSERT-VALUE)" t nil)

(autoload 'haskell-mode-jump-to-def-or-tag "haskell-commands" "\
Jump to the definition.
Jump to definition of identifier at point by consulting GHCi, or
tag table as fallback.

Remember: If GHCi is busy doing something, this will delay, but
it will always be accurate, in contrast to tags, which always
work but are not always accurate.
If the definition or tag is found, the location from which you jumped
will be pushed onto `xref--marker-ring', so you can return to that
position with `xref-pop-marker-stack'.

\(fn &optional NEXT-P)" t nil)

(autoload 'haskell-mode-goto-loc "haskell-commands" "\
Go to the location of the thing at point.
Requires the :loc-at command from GHCi.

\(fn)" t nil)

(autoload 'haskell-mode-jump-to-def "haskell-commands" "\
Jump to definition of identifier IDENT at point.

\(fn IDENT)" t nil)

(autoload 'haskell-process-cd "haskell-commands" "\
Change directory.

\(fn &optional NOT-INTERACTIVE)" t nil)

(autoload 'haskell-process-cabal-macros "haskell-commands" "\
Send the cabal macros string.

\(fn)" t nil)

(autoload 'haskell-mode-show-type-at "haskell-commands" "\
Show type of the thing at point or within active region asynchronously.
This function requires GHCi-ng and `:set +c` option enabled by
default (please follow GHCi-ng README available at URL
`https://github.com/chrisdone/ghci-ng').

\\<haskell-interactive-mode-map>
To make this function works sometimes you need to load the file in REPL
first using command `haskell-process-load-or-reload' bound to
\\[haskell-process-load-or-reload].

Optional argument INSERT-VALUE indicates that
recieved type signature should be inserted (but only if nothing
happened since function invocation).

\(fn &optional INSERT-VALUE)" t nil)

(autoload 'haskell-process-generate-tags "haskell-commands" "\
Regenerate the TAGS table.
If optional AND-THEN-FIND-THIS-TAG argument is present it is used with
function `xref-find-definitions' after new table was generated.

\(fn &optional AND-THEN-FIND-THIS-TAG)" t nil)

(autoload 'haskell-process-unignore "haskell-commands" "\
Unignore any ignored files.
Do not ignore files that were specified as being ignored by the
inferior GHCi process.

\(fn)" t nil)

(autoload 'haskell-session-change-target "haskell-commands" "\
Set the build TARGET for cabal REPL.

\(fn TARGET)" t nil)

(autoload 'haskell-mode-stylish-buffer "haskell-commands" "\
Apply stylish-haskell to the current buffer.

\(fn)" t nil)

(autoload 'haskell-mode-find-uses "haskell-commands" "\
Find use cases of the identifier at point and highlight them all.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "haskell-compile" "haskell-compile.el" (22021
;;;;;;  61696 89140 0))
;;; Generated autoloads from haskell-compile.el

(let ((loads (get 'haskell-compile 'custom-loads))) (if (member '"haskell-compile" loads) nil (put 'haskell-compile 'custom-loads (cons '"haskell-compile" loads))))

(defvar haskell-compile-cabal-build-command "cd %s && cabal build --ghc-option=-ferror-spans" "\
Default build command to use for `haskell-cabal-build' when a cabal file is detected.
The `%s' placeholder is replaced by the cabal package top folder.")

(custom-autoload 'haskell-compile-cabal-build-command "haskell-compile" t)

(defvar haskell-compile-cabal-build-alt-command "cd %s && cabal clean -s && cabal build --ghc-option=-ferror-spans" "\
Alternative build command to use when `haskell-cabal-build' is called with a negative prefix argument.
The `%s' placeholder is replaced by the cabal package top folder.")

(custom-autoload 'haskell-compile-cabal-build-alt-command "haskell-compile" t)

(defvar haskell-compile-command "ghc -Wall -ferror-spans -fforce-recomp -c %s" "\
Default build command to use for `haskell-cabal-build' when no cabal file is detected.
The `%s' placeholder is replaced by the current buffer's filename.")

(custom-autoload 'haskell-compile-command "haskell-compile" t)

(defvar haskell-compile-ghc-filter-linker-messages t "\
Filter out unremarkable \"Loading package...\" linker messages during compilation.")

(custom-autoload 'haskell-compile-ghc-filter-linker-messages "haskell-compile" t)

(autoload 'haskell-compile "haskell-compile" "\
Compile the Haskell program including the current buffer.
Tries to locate the next cabal description in current or parent
folders via `haskell-cabal-find-dir' and if found, invoke
`haskell-compile-cabal-build-command' from the cabal package root
folder. If no cabal package could be detected,
`haskell-compile-command' is used instead.

If prefix argument EDIT-COMMAND is non-nil (and not a negative
prefix `-'), `haskell-compile' prompts for custom compile
command.

If EDIT-COMMAND contains the negative prefix argument `-',
`haskell-compile' calls the alternative command defined in
`haskell-compile-cabal-build-alt-command' if a cabal package was
detected.

`haskell-compile' uses `haskell-compilation-mode' which is
derived from `compilation-mode'. See Info
node `(haskell-mode)compilation' for more details.

\(fn &optional EDIT-COMMAND)" t nil)

;;;***

;;;### (autoloads nil "haskell-complete-module" "haskell-complete-module.el"
;;;;;;  (22021 61696 93140 0))
;;; Generated autoloads from haskell-complete-module.el

(defvar haskell-complete-module-preferred 'nil "\
Override ordering of module results by specifying preferred modules.")

(custom-autoload 'haskell-complete-module-preferred "haskell-complete-module" t)

(defvar haskell-complete-module-max-display 10 "\
Maximum items to display in minibuffer.")

(custom-autoload 'haskell-complete-module-max-display "haskell-complete-module" t)

;;;***

;;;### (autoloads nil "haskell-customize" "haskell-customize.el"
;;;;;;  (22021 61696 85140 0))
;;; Generated autoloads from haskell-customize.el

(defvar haskell-process-load-or-reload-prompt nil "\
Nil means there will be no prompts on starting REPL. Defaults will be accepted.")

(custom-autoload 'haskell-process-load-or-reload-prompt "haskell-customize" t)

(let ((loads (get 'haskell 'custom-loads))) (if (member '"haskell-customize" loads) nil (put 'haskell 'custom-loads (cons '"haskell-customize" loads))))

(defvar haskell-completing-read-function 'ido-completing-read "\
Default function to use for completion.")

(custom-autoload 'haskell-completing-read-function "haskell-customize" t)

(defvar haskell-process-type 'auto "\
The inferior Haskell process type to use.

When set to 'auto (the default), the directory contents and
available programs will be used to make a best guess at the
process type:

If the project directory or one of its parents contains a
\"cabal.sandbox.config\" file, then cabal-repl will be used.

If there's a \"stack.yaml\" file and the \"stack\" executable can
be located, then stack-ghci will be used.

Otherwise if there's a *.cabal file, cabal-repl will be used.

If none of the above apply, ghci will be used.")

(custom-autoload 'haskell-process-type "haskell-customize" t)

(defvar haskell-process-wrapper-function #'identity "\
Wrap or transform haskell process commands using this function.

Can be set to a custom function which takes a list of arguments
and returns a possibly-modified list.

The following example function arranges for all haskell process
commands to be started in the current nix-shell environment:

  (lambda (argv) (append (list \"nix-shell\" \"-I\" \".\" \"--command\" )
                    (list (mapconcat 'identity argv \" \"))))

See Info Node `(emacs)Directory Variables' for a way to set this option on
a per-project basis.")

(custom-autoload 'haskell-process-wrapper-function "haskell-customize" t)

(defvar haskell-ask-also-kill-buffers t "\
Ask whether to kill all associated buffers when a session
 process is killed.")

(custom-autoload 'haskell-ask-also-kill-buffers "haskell-customize" t)

(defvar haskell-doc-prettify-types t "\
Replace some parts of types with Unicode characters like \"∷\"
when showing type information about symbols.")

(custom-autoload 'haskell-doc-prettify-types "haskell-customize" t)

(let ((loads (get 'haskell-interactive 'custom-loads))) (if (member '"haskell-customize" loads) nil (put 'haskell-interactive 'custom-loads (cons '"haskell-customize" loads))))

(defvar haskell-process-path-ghci "ghci" "\
The path for starting ghci.")

(custom-autoload 'haskell-process-path-ghci "haskell-customize" t)

(defvar haskell-process-path-cabal "cabal" "\
Path to the `cabal' executable.")

(custom-autoload 'haskell-process-path-cabal "haskell-customize" t)

(defvar haskell-process-path-stack "stack" "\
The path for starting stack.")

(custom-autoload 'haskell-process-path-stack "haskell-customize" t)

(defvar haskell-process-args-ghci '("-ferror-spans") "\
Any arguments for starting ghci.")

(custom-autoload 'haskell-process-args-ghci "haskell-customize" t)

(defvar haskell-process-args-cabal-repl '("--ghc-option=-ferror-spans") "\
Additional arguments for `cabal repl' invocation.
Note: The settings in `haskell-process-path-ghci' and
`haskell-process-args-ghci' are not automatically reused as `cabal repl'
currently invokes `ghc --interactive'. Use
`--with-ghc=<path-to-executable>' if you want to use a different
interactive GHC frontend; use `--ghc-option=<ghc-argument>' to
pass additional flags to `ghc'.")

(custom-autoload 'haskell-process-args-cabal-repl "haskell-customize" t)

(defvar haskell-process-args-stack-ghci '("--ghc-options=-ferror-spans") "\
Additional arguments for `stack ghci' invocation.")

(custom-autoload 'haskell-process-args-stack-ghci "haskell-customize" t)

(defvar haskell-process-do-cabal-format-string ":!cd %s && %s" "\
The way to run cabal comands. It takes two arguments -- the directory and the command.
See `haskell-process-do-cabal' for more details.")

(custom-autoload 'haskell-process-do-cabal-format-string "haskell-customize" t)

(defvar haskell-process-log nil "\
Enable debug logging to \"*haskell-process-log*\" buffer.")

(custom-autoload 'haskell-process-log "haskell-customize" t)

(defvar haskell-process-show-debug-tips t "\
Show debugging tips when starting the process.")

(custom-autoload 'haskell-process-show-debug-tips "haskell-customize" t)

(defvar haskell-notify-p nil "\
Notify using notifications.el (if loaded)?")

(custom-autoload 'haskell-notify-p "haskell-customize" t)

(defvar haskell-process-suggest-no-warn-orphans t "\
Suggest adding -fno-warn-orphans pragma to file when getting orphan warnings.")

(custom-autoload 'haskell-process-suggest-no-warn-orphans "haskell-customize" t)

(defvar haskell-process-suggest-hoogle-imports nil "\
Suggest to add import statements using Hoogle as a backend.")

(custom-autoload 'haskell-process-suggest-hoogle-imports "haskell-customize" t)

(defvar haskell-process-suggest-hayoo-imports nil "\
Suggest to add import statements using Hayoo as a backend.")

(custom-autoload 'haskell-process-suggest-hayoo-imports "haskell-customize" t)

(defvar haskell-process-hayoo-query-url "http://hayoo.fh-wedel.de/json/?query=%s" "\
Query url for json hayoo results.")

(custom-autoload 'haskell-process-hayoo-query-url "haskell-customize" t)

(defvar haskell-process-suggest-haskell-docs-imports nil "\
Suggest to add import statements using haskell-docs as a backend.")

(custom-autoload 'haskell-process-suggest-haskell-docs-imports "haskell-customize" t)

(defvar haskell-process-suggest-add-package t "\
Suggest to add packages to your .cabal file when Cabal says it
is a member of the hidden package, blah blah.")

(custom-autoload 'haskell-process-suggest-add-package "haskell-customize" t)

(defvar haskell-process-suggest-language-pragmas t "\
Suggest adding LANGUAGE pragmas recommended by GHC.")

(custom-autoload 'haskell-process-suggest-language-pragmas "haskell-customize" t)

(defvar haskell-process-suggest-remove-import-lines nil "\
Suggest removing import lines as warned by GHC.")

(custom-autoload 'haskell-process-suggest-remove-import-lines "haskell-customize" t)

(defvar haskell-process-suggest-overloaded-strings t "\
Suggest adding OverloadedStrings pragma to file when getting type mismatches with [Char].")

(custom-autoload 'haskell-process-suggest-overloaded-strings "haskell-customize" t)

(defvar haskell-process-check-cabal-config-on-load t "\
Check changes cabal config on loading Haskell files and
restart the GHCi process if changed..")

(custom-autoload 'haskell-process-check-cabal-config-on-load "haskell-customize" t)

(defvar haskell-process-prompt-restart-on-cabal-change t "\
Ask whether to restart the GHCi process when the Cabal file
has changed?")

(custom-autoload 'haskell-process-prompt-restart-on-cabal-change "haskell-customize" t)

(defvar haskell-process-auto-import-loaded-modules nil "\
Auto import the modules reported by GHC to have been loaded?")

(custom-autoload 'haskell-process-auto-import-loaded-modules "haskell-customize" t)

(defvar haskell-process-reload-with-fbytecode nil "\
When using -fobject-code, auto reload with -fbyte-code (and
then restore the -fobject-code) so that all module info and
imports become available?")

(custom-autoload 'haskell-process-reload-with-fbytecode "haskell-customize" t)

(defvar haskell-process-use-presentation-mode nil "\
Use presentation mode to show things like type info instead of
  printing to the message area.")

(custom-autoload 'haskell-process-use-presentation-mode "haskell-customize" t)

(defvar haskell-process-suggest-restart t "\
Suggest restarting the process when it has died")

(custom-autoload 'haskell-process-suggest-restart "haskell-customize" t)

(defvar haskell-interactive-mode-scroll-to-bottom nil "\
Scroll to bottom in the REPL always.")

(custom-autoload 'haskell-interactive-mode-scroll-to-bottom "haskell-customize" t)

(defvar haskell-interactive-popup-errors t "\
Popup errors in a separate buffer.")

(custom-autoload 'haskell-interactive-popup-errors "haskell-customize" t)

(defvar haskell-interactive-mode-collapse nil "\
Collapse printed results.")

(custom-autoload 'haskell-interactive-mode-collapse "haskell-customize" t)

(defvar haskell-interactive-types-for-show-ambiguous t "\
Show types when there's no Show instance or there's an
ambiguous class constraint.")

(custom-autoload 'haskell-interactive-types-for-show-ambiguous "haskell-customize" t)

(defvar haskell-interactive-mode-eval-mode nil "\
Use the given mode's font-locking to render some text.")

(custom-autoload 'haskell-interactive-mode-eval-mode "haskell-customize" t)

(defvar haskell-interactive-mode-hide-multi-line-errors nil "\
Hide collapsible multi-line compile messages by default.")

(custom-autoload 'haskell-interactive-mode-hide-multi-line-errors "haskell-customize" t)

(defvar haskell-interactive-mode-delete-superseded-errors t "\
Whether to delete compile messages superseded by recompile/reloads.")

(custom-autoload 'haskell-interactive-mode-delete-superseded-errors "haskell-customize" t)

(defvar haskell-interactive-mode-include-file-name t "\
Include the file name of the module being compiled when
printing compilation messages.")

(custom-autoload 'haskell-interactive-mode-include-file-name "haskell-customize" t)

(defvar haskell-import-mapping 'nil "\
Support a mapping from module to import lines.

E.g. '((\"Data.Map\" . \"import qualified Data.Map as M
import Data.Map (Map)
\"))

This will import

import qualified Data.Map as M
import Data.Map (Map)

when Data.Map is the candidate.

")

(custom-autoload 'haskell-import-mapping "haskell-customize" t)

(defvar haskell-language-extensions 'nil "\
Language extensions in use. Should be in format: -XFoo,
-XNoFoo etc. The idea is that various tools written with HSE (or
any haskell-mode code that needs to be aware of syntactical
properties; such as an indentation mode) that don't know what
extensions to use can use this variable. Examples: hlint,
hindent, structured-haskell-mode, tool-de-jour, etc.

You can set this per-project with a .dir-locals.el file, in the
same vein as `haskell-indent-spaces'.")

(custom-autoload 'haskell-language-extensions "haskell-customize" t)

;;;***

;;;### (autoloads nil "haskell-debug" "haskell-debug.el" (22021 61695
;;;;;;  961140 0))
;;; Generated autoloads from haskell-debug.el

(let ((loads (get 'haskell-debug 'custom-loads))) (if (member '"haskell-debug" loads) nil (put 'haskell-debug 'custom-loads (cons '"haskell-debug" loads))))

(defface haskell-debug-warning-face '((t :inherit 'compilation-warning)) "\
Face for warnings." :group (quote haskell-debug))

(defface haskell-debug-trace-number-face '((t :weight bold :background "#f5f5f5")) "\
Face for numbers in backtrace." :group (quote haskell-debug))

(defface haskell-debug-newline-face '((t :weight bold :background "#f0f0f0")) "\
Face for newlines in trace steps." :group (quote haskell-debug))

(defface haskell-debug-keybinding-face '((t :inherit 'font-lock-type-face :weight bold)) "\
Face for keybindings." :group (quote haskell-debug))

(defface haskell-debug-heading-face '((t :inherit 'font-lock-keyword-face)) "\
Face for headings." :group (quote haskell-debug))

(defface haskell-debug-muted-face '((t :foreground "#999")) "\
Face for muteds." :group (quote haskell-debug))

;;;***

;;;### (autoloads nil "haskell-decl-scan" "haskell-decl-scan.el"
;;;;;;  (22021 61696 149140 0))
;;; Generated autoloads from haskell-decl-scan.el

(let ((loads (get 'haskell-decl-scan 'custom-loads))) (if (member '"haskell-decl-scan" loads) nil (put 'haskell-decl-scan 'custom-loads (cons '"haskell-decl-scan" loads))))

(defvar haskell-decl-scan-bindings-as-variables nil "\
Whether to put top-level value bindings into a \"Variables\" category.")

(custom-autoload 'haskell-decl-scan-bindings-as-variables "haskell-decl-scan" t)

(defvar haskell-decl-scan-add-to-menubar t "\
Whether to add a \"Declarations\" menu entry to menu bar.")

(custom-autoload 'haskell-decl-scan-add-to-menubar "haskell-decl-scan" t)

(autoload 'haskell-ds-create-imenu-index "haskell-decl-scan" "\
Function for finding `imenu' declarations in Haskell mode.
Finds all declarations (classes, variables, imports, instances and
datatypes) in a Haskell file for the `imenu' package.

\(fn)" nil nil)

(autoload 'turn-on-haskell-decl-scan "haskell-decl-scan" "\
Unconditionally activate `haskell-decl-scan-mode'.

\(fn)" t nil)

(autoload 'haskell-decl-scan-mode "haskell-decl-scan" "\
Toggle Haskell declaration scanning minor mode on or off.
With a prefix argument ARG, enable minor mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil, and toggle it if ARG is `toggle'.

See also info node `(haskell-mode)haskell-decl-scan-mode' for
more details about this minor mode.

Top-level declarations are scanned and listed in the menu item
\"Declarations\" (if enabled via option
`haskell-decl-scan-add-to-menubar').  Selecting an item from this
menu will take point to the start of the declaration.

\\[beginning-of-defun] and \\[end-of-defun] move forward and backward to the start of a declaration.

This may link with `haskell-doc-mode'.

For non-literate and LaTeX-style literate scripts, we assume the
common convention that top-level declarations start at the first
column.  For Bird-style literate scripts, we assume the common
convention that top-level declarations start at the third column,
ie. after \"> \".

Anything in `font-lock-comment-face' is not considered for a
declaration.  Therefore, using Haskell font locking with comments
coloured in `font-lock-comment-face' improves declaration scanning.

Literate Haskell scripts are supported: If the value of
`haskell-literate' (set automatically by `literate-haskell-mode')
is `bird', a Bird-style literate script is assumed.  If it is nil
or `tex', a non-literate or LaTeX-style literate script is
assumed, respectively.

Invokes `haskell-decl-scan-mode-hook' on activation.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil "haskell-doc" "haskell-doc.el" (22021 61696
;;;;;;  25140 0))
;;; Generated autoloads from haskell-doc.el

(let ((loads (get 'haskell-doc 'custom-loads))) (if (member '"haskell-doc" loads) nil (put 'haskell-doc 'custom-loads (cons '"haskell-doc" loads))))

(defvar haskell-doc-show-global-types nil "\
If non-nil, search for the types of global functions by loading the files.
This variable is buffer-local.")

(custom-autoload 'haskell-doc-show-global-types "haskell-doc" t)

(defvar haskell-doc-show-reserved t "\
If non-nil, show a documentation string for reserved ids.
This variable is buffer-local.")

(custom-autoload 'haskell-doc-show-reserved "haskell-doc" t)

(defvar haskell-doc-show-prelude t "\
If non-nil, show a documentation string for prelude functions.
This variable is buffer-local.")

(custom-autoload 'haskell-doc-show-prelude "haskell-doc" t)

(defvar haskell-doc-show-strategy t "\
If non-nil, show a documentation string for strategies.
This variable is buffer-local.")

(custom-autoload 'haskell-doc-show-strategy "haskell-doc" t)

(defvar haskell-doc-show-user-defined t "\
If non-nil, show a documentation string for user defined ids.
This variable is buffer-local.")

(custom-autoload 'haskell-doc-show-user-defined "haskell-doc" t)

(defvar haskell-doc-chop-off-context t "\
If non-nil eliminate the context part in a Haskell type.")

(custom-autoload 'haskell-doc-chop-off-context "haskell-doc" t)

(defvar haskell-doc-chop-off-fctname nil "\
If non-nil omit the function name and show only the type.")

(custom-autoload 'haskell-doc-chop-off-fctname "haskell-doc" t)

(autoload 'haskell-doc-mode "haskell-doc" "\
Enter `haskell-doc-mode' for showing fct types in the echo area.
See variable docstring.

\(fn &optional ARG)" t nil)

(defalias 'turn-on-haskell-doc-mode 'haskell-doc-mode)

(defalias 'turn-on-haskell-doc 'haskell-doc-mode)

(autoload 'haskell-doc-current-info "haskell-doc" "\
Return the info about symbol at point.
Meant for `eldoc-documentation-function'.

\(fn)" nil nil)

(autoload 'haskell-doc-show-type "haskell-doc" "\
Show the type of the function near point.
For the function under point, show the type in the echo area.
This information is extracted from the `haskell-doc-prelude-types' alist
of prelude functions and their types, or from the local functions in the
current buffer.

\(fn &optional SYM)" t nil)

;;;***

;;;### (autoloads nil "haskell-font-lock" "haskell-font-lock.el"
;;;;;;  (22021 61696 53140 0))
;;; Generated autoloads from haskell-font-lock.el

(defvar haskell-font-lock-symbols nil "\
Display \\ and -> and such using symbols in fonts.

This may sound like a neat trick, but be extra careful: it changes the
alignment and can thus lead to nasty surprises w.r.t layout.")

(custom-autoload 'haskell-font-lock-symbols "haskell-font-lock" t)

(defvar haskell-font-lock-symbols-alist '(("\\" . "λ") ("not" . "¬") ("->" . "→") ("<-" . "←") ("=>" . "⇒") ("()" . "∅") ("==" . "≡") ("/=" . "≢") (">=" . "≥") ("<=" . "≤") ("!!" . "‼") ("&&" . "∧") ("||" . "∨") ("sqrt" . "√") ("undefined" . "⊥") ("pi" . "π") ("~>" . "⇝") ("-<" . "↢") ("::" . "∷") ("." "∘" haskell-font-lock-dot-is-not-composition) ("forall" . "∀")) "\
Alist mapping Haskell symbols to chars.

Each element has the form (STRING . COMPONENTS) or (STRING
COMPONENTS PREDICATE).

STRING is the Haskell symbol.
COMPONENTS is a representation specification suitable as an argument to
`compose-region'.
PREDICATE if present is a function of one argument (the start position
of the symbol) which should return non-nil if this mapping should
be disabled at that position.")

(custom-autoload 'haskell-font-lock-symbols-alist "haskell-font-lock" t)

(defface haskell-keyword-face '((t :inherit font-lock-keyword-face)) "\
Face used to highlight Haskell keywords." :group (quote haskell))

(defface haskell-constructor-face '((t :inherit font-lock-type-face)) "\
Face used to highlight Haskell constructors." :group (quote haskell))

(defface haskell-operator-face '((t :inherit font-lock-variable-name-face)) "\
Face used to highlight Haskell operators." :group (quote haskell))

(defface haskell-pragma-face '((t :inherit font-lock-preprocessor-face)) "\
Face used to highlight Haskell pragmas." :group (quote haskell))

(defface haskell-literate-comment-face '((t :inherit font-lock-doc-face)) "\
Face with which to fontify literate comments.
Inherit from `default' to avoid fontification of them." :group (quote haskell))

(autoload 'haskell-font-lock-choose-keywords "haskell-font-lock" "\


\(fn)" nil nil)

;;;***

;;;### (autoloads nil "haskell-indent" "haskell-indent.el" (22021
;;;;;;  61696 129140 0))
;;; Generated autoloads from haskell-indent.el

(let ((loads (get 'haskell-indent 'custom-loads))) (if (member '"haskell-indent" loads) nil (put 'haskell-indent 'custom-loads (cons '"haskell-indent" loads))))

(defvar haskell-indent-offset 4 "\
Indentation of Haskell statements with respect to containing block.")

(custom-autoload 'haskell-indent-offset "haskell-indent" t)

(defvar haskell-indent-literate-Bird-default-offset 1 "\
Default number of blanks after > in a Bird style literate script.")

(custom-autoload 'haskell-indent-literate-Bird-default-offset "haskell-indent" t)

(defvar haskell-indent-rhs-align-column 0 "\
Column on which to align right-hand sides (use 0 for ad-hoc alignment).")

(custom-autoload 'haskell-indent-rhs-align-column "haskell-indent" t)

(defvar haskell-indent-look-past-empty-line t "\
If nil, indentation engine will not look past an empty line for layout points.")

(custom-autoload 'haskell-indent-look-past-empty-line "haskell-indent" t)

(defvar haskell-indent-thenelse 0 "\
If non-nil, \"then\" and \"else\" are indented.
This is necessary in the \"do\" layout under Haskell-98.
See http://hackage.haskell.org/trac/haskell-prime/wiki/DoAndIfThenElse")

(custom-autoload 'haskell-indent-thenelse "haskell-indent" t)

(defvar haskell-indent-after-keywords '(("where" 2 0) ("of" 2) ("do" 2) ("mdo" 2) ("rec" 2) ("in" 2 0) ("{" 2) "if" "then" "else" "let") "\
Keywords after which indentation should be indented by some offset.
Each keyword info can have the following forms:

   KEYWORD | (KEYWORD OFFSET [OFFSET-HANGING])

If absent OFFSET-HANGING defaults to OFFSET.
If absent OFFSET defaults to `haskell-indent-offset'.

OFFSET-HANGING is the offset to use in the case where the keyword
is at the end of an otherwise-non-empty line.")

(custom-autoload 'haskell-indent-after-keywords "haskell-indent" t)

(defvar haskell-indent-dont-hang '("(") "\
Lexemes that should never be considered as hanging.")

(custom-autoload 'haskell-indent-dont-hang "haskell-indent" t)

(autoload 'turn-on-haskell-indent "haskell-indent" "\
Turn on ``intelligent'' Haskell indentation mode.

\(fn)" nil nil)

(autoload 'haskell-indent-mode "haskell-indent" "\
``Intelligent'' Haskell indentation mode.
This deals with the layout rule of Haskell.
\\[haskell-indent-cycle] starts the cycle which proposes new
possibilities as long as the TAB key is pressed.  Any other key
or mouse click terminates the cycle and is interpreted except for
RET which merely exits the cycle.
Other special keys are:
    \\[haskell-indent-insert-equal]
      inserts an =
    \\[haskell-indent-insert-guard]
      inserts an |
    \\[haskell-indent-insert-otherwise]
      inserts an | otherwise =
these functions also align the guards and rhs of the current definition
    \\[haskell-indent-insert-where]
      inserts a where keyword
    \\[haskell-indent-align-guards-and-rhs]
      aligns the guards and rhs of the region
    \\[haskell-indent-put-region-in-literate]
      makes the region a piece of literate code in a literate script

Invokes `haskell-indent-hook' if not nil.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil "haskell-indentation" "haskell-indentation.el"
;;;;;;  (22021 61696 37140 0))
;;; Generated autoloads from haskell-indentation.el

(let ((loads (get 'haskell-indentation 'custom-loads))) (if (member '"haskell-indentation" loads) nil (put 'haskell-indentation 'custom-loads (cons '"haskell-indentation" loads))))

(defvar haskell-indentation-show-indentations nil "\
If t the current line's indentation points will be showed as
underscore overlays in new haskell-mode buffers.  Use
`haskell-indentation-enable-show-indentations' and
`haskell-indentation-disable-show-indentations' to switch the
behavior for already existing buffers.")

(custom-autoload 'haskell-indentation-show-indentations "haskell-indentation" t)

(defvar haskell-indentation-show-indentations-after-eol nil "\
If t, try to show indentation points after the end of line.
This requires strange overlay hacks and can collide with other
modes (e.g. fill-column-indicator).")

(custom-autoload 'haskell-indentation-show-indentations-after-eol "haskell-indentation" t)

(defface haskell-indentation-show-normal-face '((t :underline t)) "\
Default face for indentations overlay." :group (quote haskell-indentation))

(defface haskell-indentation-show-hl-line-face '((t :underline t :inherit hl-line)) "\
Face used for indentations overlay after EOL if `hl-line-mode'
is enabled." :group (quote haskell-indentation))

(defvar haskell-indentation-indent-leftmost t "\
Indent to the left margin after certain keywords.
For example after \"let .. in\", \"case .. of\").  If set to t it
will only indent to the left.  If nil only relative to the
containing expression.  If set to the symbol 'both then both
positions are allowed.")

(custom-autoload 'haskell-indentation-indent-leftmost "haskell-indentation" t)

(defvar haskell-indentation-layout-offset 2 "\
Extra indentation to add before expressions in a Haskell layout list.")

(custom-autoload 'haskell-indentation-layout-offset "haskell-indentation" t)

(defvar haskell-indentation-starter-offset 2 "\
Extra indentation after an opening keyword (e.g. \"let\").")

(custom-autoload 'haskell-indentation-starter-offset "haskell-indentation" t)

(defvar haskell-indentation-left-offset 2 "\
Extra indentation after an indentation to the left (e.g. after \"do\").")

(custom-autoload 'haskell-indentation-left-offset "haskell-indentation" t)

(defvar haskell-indentation-ifte-offset 2 "\
Extra indentation after the keywords \"if\", \"then\", or \"else\".")

(custom-autoload 'haskell-indentation-ifte-offset "haskell-indentation" t)

(defvar haskell-indentation-where-pre-offset 2 "\
Extra indentation before the keyword \"where\".")

(custom-autoload 'haskell-indentation-where-pre-offset "haskell-indentation" t)

(defvar haskell-indentation-where-post-offset 2 "\
Extra indentation after the keyword \"where\".")

(custom-autoload 'haskell-indentation-where-post-offset "haskell-indentation" t)

(autoload 'haskell-indentation-mode "haskell-indentation" "\
Haskell indentation mode that deals with the layout rule.
It rebinds RET, DEL and BACKSPACE, so that indentations can be
set and deleted as if they were real tabs.  It supports
`auto-fill-mode'.

It is possible to render indent stops for current line as
overlays.  Please read documentation for option
`haskell-indentation-enable-show-indentations' and option
`haskell-indentation-show-indentations-after-eol'.  These options
were disabled by default because in most cases occurs overlay
clashing with other modes.

\(fn &optional ARG)" t nil)

(autoload 'turn-on-haskell-indentation "haskell-indentation" "\
Turn on the haskell-indentation minor mode.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "haskell-interactive-mode" "haskell-interactive-mode.el"
;;;;;;  (22021 61695 969140 0))
;;; Generated autoloads from haskell-interactive-mode.el

(defface haskell-interactive-face-prompt '((t :inherit font-lock-function-name-face)) "\
Face for the prompt." :group (quote haskell-interactive))

(defface haskell-interactive-face-compile-error '((t :inherit compilation-error)) "\
Face for compile errors." :group (quote haskell-interactive))

(defface haskell-interactive-face-compile-warning '((t :inherit compilation-warning)) "\
Face for compiler warnings." :group (quote haskell-interactive))

(defface haskell-interactive-face-result '((t :inherit font-lock-string-face)) "\
Face for the result." :group (quote haskell-interactive))

(defface haskell-interactive-face-garbage '((t :inherit font-lock-string-face)) "\
Face for trailing garbage after a command has completed." :group (quote haskell-interactive))

(autoload 'haskell-interactive-mode-reset-error "haskell-interactive-mode" "\
Reset the error cursor position.

\(fn SESSION)" t nil)

(autoload 'haskell-interactive-mode-echo "haskell-interactive-mode" "\
Echo a read only piece of text before the prompt.

\(fn SESSION MESSAGE &optional MODE)" nil nil)

(autoload 'haskell-process-show-repl-response "haskell-interactive-mode" "\
Send LINE to the GHCi process and echo the result in some fashion.
Result will be printed in the minibuffer or presented using
function `haskell-presentation-present', depending on variable
`haskell-process-use-presentation-mode'.

\(fn LINE)" nil nil)

;;;***

;;;### (autoloads nil "haskell-load" "haskell-load.el" (22021 61695
;;;;;;  953140 0))
;;; Generated autoloads from haskell-load.el

(defface haskell-error-face '((((supports :underline (:style wave))) :underline (:style wave :color "#dc322f")) (t :inherit error)) "\
Face used for marking error lines." :group (quote haskell-mode))

(defface haskell-warning-face '((((supports :underline (:style wave))) :underline (:style wave :color "#b58900")) (t :inherit warning)) "\
Face used for marking warning lines." :group (quote haskell-mode))

(defface haskell-hole-face '((((supports :underline (:style wave))) :underline (:style wave :color "#6c71c4")) (t :inherit warning)) "\
Face used for marking hole lines." :group (quote haskell-mode))

(autoload 'haskell-process-reload-devel-main "haskell-load" "\
Reload the module `DevelMain' and then run
`DevelMain.update'. This is for doing live update of the code of
servers or GUI applications. Put your development version of the
program in `DevelMain', and define `update' to auto-start the
program on a new thread, and use the `foreign-store' package to
access the running context across :load/:reloads in GHCi.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "haskell-menu" "haskell-menu.el" (22021 61696
;;;;;;  61140 0))
;;; Generated autoloads from haskell-menu.el

(defvar haskell-menu-buffer-name "*haskell-menu*" "\
The name of the Haskell session menu buffer")

(custom-autoload 'haskell-menu-buffer-name "haskell-menu" t)

(autoload 'haskell-menu "haskell-menu" "\
Launch the Haskell sessions menu.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "haskell-mode" "haskell-mode.el" (22021 61696
;;;;;;  121140 0))
;;; Generated autoloads from haskell-mode.el

(autoload 'haskell-version "haskell-mode" "\
Show the `haskell-mode` version in the echo area.
With prefix argument HERE, insert it at point.

\(fn &optional HERE)" t nil)

(autoload 'haskell-mode-view-news "haskell-mode" "\
Display information on recent changes to haskell-mode.

\(fn)" t nil)

(defvar haskell-literate-default 'bird "\
Default value for `haskell-literate'.
Used if the style of a literate buffer is ambiguous.  This variable should
be set to the preferred literate style.")

(custom-autoload 'haskell-literate-default "haskell-mode" t)

(defvar haskell-mode-contextual-import-completion t "\
Enable import completion on haskell-mode-contextual-space.")

(custom-autoload 'haskell-mode-contextual-import-completion "haskell-mode" t)

(autoload 'haskell-mode "haskell-mode" "\
Major mode for editing Haskell programs.

For more information aee also Info node `(haskell-mode)Getting Started'.

\\<haskell-mode-map>

Literate Haskell scripts are supported via `literate-haskell-mode'.
The variable `haskell-literate' indicates the style of the script in the
current buffer.  See the documentation on this variable for more details.

Use `haskell-version' to find out what version of Haskell mode you are
currently using.

Additional Haskell mode modules can be hooked in via `haskell-mode-hook'.

Indentation modes:

    `haskell-indentation-mode', Kristof Bastiaensen, Gergely Risko
      Intelligent semi-automatic indentation Mk2

    `haskell-indent-mode', Guy Lapalme
      Intelligent semi-automatic indentation.

    `haskell-simple-indent-mode', Graeme E Moss and Heribert Schuetz
      Simple indentation.

Interaction modes:

    `interactive-haskell-mode'
      Interact with per-project GHCi processes through a REPL and
      directory-aware sessions.

    `inf-haskell-mode'
      Interact with a GHCi process using comint-mode. Deprecated.

Other modes:

    `haskell-decl-scan-mode', Graeme E Moss
      Scans top-level declarations, and places them in a menu.

    `haskell-doc-mode', Hans-Wolfgang Loidl
      Echoes types of functions or syntax of keywords when the cursor is idle.

To activate a minor-mode, simply run the interactive command. For
example, `M-x haskell-doc-mode'. Run it again to disable it.

To enable a mode for every haskell-mode buffer, add a hook in
your Emacs configuration. To do that you can customize
`haskell-mode-hook' or add lines to your .emacs file. For
example, to enable `haskell-indent-mode' and
`interactive-haskell-mode', use the following:

    (add-hook 'haskell-mode-hook 'haskell-indentation-mode)
    (add-hook 'haskell-mode-hook 'interactive-haskell-mode)

For more details see Info node `(haskell-mode)haskell-mode-hook'.

Warning: do not enable more than one of the above indentation
modes. See Info node `(haskell-mode)indentation' for more
details.

Minor modes that work well with `haskell-mode':

- `smerge-mode': show and work with diff3 conflict markers used
  by git, svn and other version control systems.

\(fn)" t nil)

(autoload 'haskell-forward-sexp "haskell-mode" "\
Haskell specific version of `forward-sexp'.

Move forward across one balanced expression (sexp).  With ARG, do
it that many times.  Negative arg -N means move backward across N
balanced expressions.  This command assumes point is not in a
string or comment.

Note that negative arguments do not work so well.

\(fn &optional ARG)" t nil)

(autoload 'literate-haskell-mode "haskell-mode" "\
As `haskell-mode' but for literate scripts.

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.[gh]s\\'" . haskell-mode))

(add-to-list 'auto-mode-alist '("\\.l[gh]s\\'" . literate-haskell-mode))

(add-to-list 'auto-mode-alist '("\\.hsc\\'" . haskell-mode))

(add-to-list 'interpreter-mode-alist '("runghc" . haskell-mode))

(add-to-list 'interpreter-mode-alist '("runhaskell" . haskell-mode))

(add-to-list 'completion-ignored-extensions ".hi")

(defvar haskell-hoogle-command (if (executable-find "hoogle") "hoogle") "\
Name of the command to use to query Hoogle.
If nil, use the Hoogle web-site.")

(custom-autoload 'haskell-hoogle-command "haskell-mode" t)

(defvar haskell-hoogle-url "http://haskell.org/hoogle/?q=%s" "\
Default value for hoogle web site.
")

(custom-autoload 'haskell-hoogle-url "haskell-mode" t)

(autoload 'haskell-hoogle "haskell-mode" "\
Do a Hoogle search for QUERY.
When `haskell-hoogle-command' is non-nil, this command runs
that.  Otherwise, it opens a hoogle search result in the browser.

If prefix argument INFO is given, then `haskell-hoogle-command'
is asked to show extra info for the items matching QUERY..

\(fn QUERY &optional INFO)" t nil)

(defalias 'hoogle 'haskell-hoogle)

(autoload 'hoogle-lookup-from-local "haskell-mode" "\
Lookup by local hoogle.

\(fn)" t nil)

(defvar haskell-hayoo-url "http://hayoo.fh-wedel.de/?query=%s" "\
Default value for hayoo web site.
")

(custom-autoload 'haskell-hayoo-url "haskell-mode" t)

(autoload 'haskell-hayoo "haskell-mode" "\
Do a Hayoo search for QUERY.

\(fn QUERY)" t nil)

(defalias 'hayoo 'haskell-hayoo)

(defvar haskell-check-command "hlint" "\
*Command used to check a Haskell file.")

(custom-autoload 'haskell-check-command "haskell-mode" t)

(defvar haskell-stylish-on-save nil "\
Whether to run stylish-haskell on the buffer before saving.")

(custom-autoload 'haskell-stylish-on-save "haskell-mode" t)

(defvar haskell-tags-on-save nil "\
Generate tags via hasktags after saving.")

(custom-autoload 'haskell-tags-on-save "haskell-mode" t)

(defvar haskell-indent-spaces 2 "\
Number of spaces to indent inwards.")

(custom-autoload 'haskell-indent-spaces "haskell-mode" t)

;;;***

;;;### (autoloads nil "haskell-modules" "haskell-modules.el" (22021
;;;;;;  61696 137140 0))
;;; Generated autoloads from haskell-modules.el

(autoload 'haskell-session-installed-modules "haskell-modules" "\
Get the modules installed in the current package set.

\(fn SESSION &optional DONTCREATE)" nil nil)

(autoload 'haskell-session-all-modules "haskell-modules" "\
Get all modules -- installed or in the current project.
If DONTCREATE is non-nil don't create a new session.

\(fn SESSION &optional DONTCREATE)" nil nil)

(autoload 'haskell-session-project-modules "haskell-modules" "\
Get the modules of the current project.
If DONTCREATE is non-nil don't create a new session.

\(fn SESSION &optional DONTCREATE)" nil nil)

;;;***

;;;### (autoloads nil "haskell-move-nested" "haskell-move-nested.el"
;;;;;;  (22021 61696 125140 0))
;;; Generated autoloads from haskell-move-nested.el

(autoload 'haskell-move-nested "haskell-move-nested" "\
Shift the nested off-side-rule block adjacent to point by COLS columns to the right.

In Transient Mark mode, if the mark is active, operate on the contents
of the region instead.

\(fn COLS)" nil nil)

(autoload 'haskell-move-nested-right "haskell-move-nested" "\
Increase indentation of the following off-side-rule block adjacent to point.

Use a numeric prefix argument to indicate amount of indentation to apply.

In Transient Mark mode, if the mark is active, operate on the contents
of the region instead.

\(fn COLS)" t nil)

(autoload 'haskell-move-nested-left "haskell-move-nested" "\
Decrease indentation of the following off-side-rule block adjacent to point.

Use a numeric prefix argument to indicate amount of indentation to apply.

In Transient Mark mode, if the mark is active, operate on the contents
of the region instead.

\(fn COLS)" t nil)

;;;***

;;;### (autoloads nil "haskell-navigate-imports" "haskell-navigate-imports.el"
;;;;;;  (22021 61696 141140 0))
;;; Generated autoloads from haskell-navigate-imports.el

(autoload 'haskell-navigate-imports "haskell-navigate-imports" "\
Cycle the Haskell import lines or return to point (with prefix arg).

\(fn &optional RETURN)" t nil)

(autoload 'haskell-navigate-imports-go "haskell-navigate-imports" "\
Go to the first line of a list of consequtive import lines. Cycles.

\(fn)" t nil)

(autoload 'haskell-navigate-imports-return "haskell-navigate-imports" "\
Return to the non-import point we were at before going to the module list.
   If we were originally at an import list, we can just cycle through easily.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "haskell-session" "haskell-session.el" (22021
;;;;;;  61696 41140 0))
;;; Generated autoloads from haskell-session.el

(autoload 'haskell-session-maybe "haskell-session" "\
Maybe get the Haskell session, return nil if there isn't one.

\(fn)" nil nil)

(autoload 'haskell-session-process "haskell-session" "\
Get the session process.

\(fn S)" nil nil)

;;;***

;;;### (autoloads nil "haskell-simple-indent" "haskell-simple-indent.el"
;;;;;;  (22021 61696 5140 0))
;;; Generated autoloads from haskell-simple-indent.el

(let ((loads (get 'haskell-simple-indent 'custom-loads))) (if (member '"haskell-simple-indent" loads) nil (put 'haskell-simple-indent 'custom-loads (cons '"haskell-simple-indent" loads))))

(autoload 'haskell-simple-indent-mode "haskell-simple-indent" "\
Simple Haskell indentation mode that uses simple heuristic.
In this minor mode, `indent-for-tab-command' (bound to <tab> by
default) will move the cursor to the next indent point in the
previous nonblank line, whereas `haskell-simple-indent-backtab'
\(bound to <backtab> by default) will move the cursor the
previous indent point.  An indent point is a non-whitespace
character following whitespace.

Runs `haskell-simple-indent-hook' on activation.

\(fn &optional ARG)" t nil)

(autoload 'turn-on-haskell-simple-indent "haskell-simple-indent" "\
Turn on function `haskell-simple-indent-mode'.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "haskell-sort-imports" "haskell-sort-imports.el"
;;;;;;  (22021 61696 73140 0))
;;; Generated autoloads from haskell-sort-imports.el

(autoload 'haskell-sort-imports "haskell-sort-imports" "\
Sort the import list at point. It sorts the current group
i.e. an import list separated by blank lines on either side.

If the region is active, it will restrict the imports to sort
within that region.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "haskell-unicode-input-method" "haskell-unicode-input-method.el"
;;;;;;  (22021 61695 997140 0))
;;; Generated autoloads from haskell-unicode-input-method.el

(autoload 'turn-on-haskell-unicode-input-method "haskell-unicode-input-method" "\
Set input method `haskell-unicode'.
See Info node `Unicode(haskell-mode)' for more details.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "highlight-uses-mode" "highlight-uses-mode.el"
;;;;;;  (22021 61696 97140 0))
;;; Generated autoloads from highlight-uses-mode.el

(autoload 'highlight-uses-mode "highlight-uses-mode" "\
Minor mode for highlighting and jumping between uses.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil "inf-haskell" "inf-haskell.el" (22021 61695
;;;;;;  957140 0))
;;; Generated autoloads from inf-haskell.el

(let ((loads (get 'inferior-haskell 'custom-loads))) (if (member '"inf-haskell" loads) nil (put 'inferior-haskell 'custom-loads (cons '"inf-haskell" loads))))

(defvar haskell-program-name (or (cond ((executable-find "hugs") "hugs \"+.\"") ((executable-find "ghci") "ghci")) "hugs \"+.\"") "\
The name of the command to start the inferior Haskell process.
The command can include arguments.")

(custom-autoload 'haskell-program-name "inf-haskell" t)

(defvar inferior-haskell-find-project-root t "\
If non-nil, try and find the project root directory of this file.
This will either look for a Cabal file or a \"module\" statement in the file.")

(custom-autoload 'inferior-haskell-find-project-root "inf-haskell" t)

(defalias 'run-haskell 'switch-to-haskell)

(autoload 'switch-to-haskell "inf-haskell" "\
Show the inferior-haskell buffer.  Start the process if needed.

\(fn &optional ARG)" t nil)

(defvar inferior-haskell-wait-and-jump nil "\
If non-nil, wait for file loading to terminate and jump to the error.")

(custom-autoload 'inferior-haskell-wait-and-jump "inf-haskell" t)

(autoload 'inferior-haskell-load-file "inf-haskell" "\
Pass the current buffer's file to the inferior haskell process.
If prefix arg \\[universal-argument] is given, just reload the previous file.

\(fn &optional RELOAD)" t nil)

(autoload 'inferior-haskell-load-and-run "inf-haskell" "\
Pass the current buffer's file to haskell and then run a COMMAND.

\(fn COMMAND)" t nil)

(autoload 'inferior-haskell-send-decl "inf-haskell" "\
Send current declaration to inferior-haskell process.

\(fn)" t nil)

(autoload 'inferior-haskell-type "inf-haskell" "\
Query the haskell process for the type of the given expression.
If optional argument `insert-value' is non-nil, insert the type above point
in the buffer.  This can be done interactively with the \\[universal-argument] prefix.
The returned info is cached for reuse by `haskell-doc-mode'.

\(fn EXPR &optional INSERT-VALUE)" t nil)

(autoload 'inferior-haskell-kind "inf-haskell" "\
Query the haskell process for the kind of the given expression.

\(fn TYPE)" t nil)

(autoload 'inferior-haskell-info "inf-haskell" "\
Query the haskell process for the info of the given expression.

\(fn SYM)" t nil)

(autoload 'inferior-haskell-find-definition "inf-haskell" "\
Attempt to locate and jump to the definition of the given expression.

\(fn SYM)" t nil)

(defvar inferior-haskell-use-web-docs 'fallback "\
Whether to use the online documentation.  Possible values:
`never', meaning always use local documentation, unless the local
file doesn't exist, when do nothing, `fallback', which means only
use the online documentation when the local file doesn't exist,
or `always', meaning always use the online documentation,
regardless of existance of local files.  Default is `fallback'.")

(custom-autoload 'inferior-haskell-use-web-docs "inf-haskell" t)

(defvar inferior-haskell-web-docs-base "http://haskell.org/ghc/docs/latest/html/libraries/" "\
The base URL of the online libraries documentation.
This will only be used if the value of `inferior-haskell-use-web-docs'
is `always' or `fallback'.")

(custom-autoload 'inferior-haskell-web-docs-base "inf-haskell" t)

(defvar haskell-package-manager-name "ghc-pkg" "\
Name of the program to consult regarding package details.")

(custom-autoload 'haskell-package-manager-name "inf-haskell" t)

(defvar haskell-package-conf-file (condition-case nil (with-temp-buffer (call-process "ghc" nil t nil "--print-libdir") (expand-file-name "package.conf" (buffer-substring (point-min) (1- (point-max))))) (error nil)) "\
Where the package configuration file for the package manager resides.
By default this is set to `ghc --print-libdir`/package.conf.")

(custom-autoload 'haskell-package-conf-file "inf-haskell" t)

(defvar inferior-haskell-module-alist-file (expand-file-name (concat "inf-haskell-module-alist-" (number-to-string (user-uid))) temporary-file-directory) "\
Where to save the module -> package lookup table.
Set this to nil to never cache to a file.")

(custom-autoload 'inferior-haskell-module-alist-file "inf-haskell" t)

(autoload 'inferior-haskell-find-haddock "inf-haskell" "\
Find and open the Haddock documentation of SYM.
Make sure to load the file into GHCi or Hugs first by using C-c C-l.
Only works for functions in a package installed with ghc-pkg, or
whatever the value of `haskell-package-manager-name' is.

This function needs to find which package a given module belongs
to.  In order to do this, it computes a module-to-package lookup
alist, which is expensive to compute (it takes upwards of five
seconds with more than about thirty installed packages).  As a
result, we cache it across sessions using the cache file
referenced by `inferior-haskell-module-alist-file'. We test to
see if this is newer than `haskell-package-conf-file' every time
we load it.

\(fn SYM)" t nil)

(autoload 'inf-haskell-mode "inf-haskell" "\
Minor mode for enabling inf-haskell process interaction.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil "w3m-haddock" "w3m-haddock.el" (22021 61696
;;;;;;  105140 0))
;;; Generated autoloads from w3m-haddock.el

(defface w3m-haddock-heading-face '((((class color)) :inherit highlight)) "\
Face for quarantines." :group (quote haskell))

(defvar haskell-w3m-haddock-dirs '("~/.cabal/share/doc/") "\
The path to your cabal documentation dir. It should contain
directories of package-name-x.x.

You can rebind this if you're using hsenv by adding it to your
.dir-locals.el in your project root. E.g.

    ((haskell-mode . ((haskell-w3m-haddock-dirs . (\"/home/chris/Projects/foobar/.hsenv/cabal/share/doc\")))))

")

(custom-autoload 'haskell-w3m-haddock-dirs "w3m-haddock" t)

;;;***

;;;### (autoloads nil nil ("haskell-collapse.el" "haskell-compat.el"
;;;;;;  "haskell-completions.el" "haskell-lexeme.el" "haskell-mode-pkg.el"
;;;;;;  "haskell-package.el" "haskell-presentation-mode.el" "haskell-process.el"
;;;;;;  "haskell-repl.el" "haskell-sandbox.el" "haskell-string.el"
;;;;;;  "haskell-utils.el") (22021 61696 158721 937000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; haskell-mode-autoloads.el ends here
