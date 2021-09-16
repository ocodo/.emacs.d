;;; cargo-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "cargo" "cargo.el" (0 0 0 0))
;;; Generated autoloads from cargo.el

(autoload 'cargo-minor-mode "cargo" "\
Cargo minor mode. Used to hold keybindings for cargo-mode.

If called interactively, enable Cargo minor mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

\\{cargo-minor-mode-command-map}

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "cargo" '("cargo-m")))

;;;***

;;;### (autoloads nil "cargo-process" "cargo-process.el" (0 0 0 0))
;;; Generated autoloads from cargo-process.el

(autoload 'cargo-process-bench "cargo-process" "\
Run the Cargo bench command.
With the prefix argument, modify the command's invocation.
Cargo: Run the benchmarks." t nil)

(autoload 'cargo-process-build "cargo-process" "\
Run the Cargo build command.
With the prefix argument, modify the command's invocation.
Cargo: Compile the current project." t nil)

(autoload 'cargo-process-clean "cargo-process" "\
Run the Cargo clean command.
With the prefix argument, modify the command's invocation.
Cargo: Remove the target directory." t nil)

(autoload 'cargo-process-doc "cargo-process" "\
Run the Cargo doc command.
With the prefix argument, modify the command's invocation.
Cargo: Build this project's and its dependencies' documentation." t nil)

(autoload 'cargo-process-doc-open "cargo-process" "\
Run the Cargo doc command with the --open switch.
With the prefix argument, modify the command's invocation.
Cargo: Open this project's documentation." t nil)

(autoload 'cargo-process-new "cargo-process" "\
Run the Cargo new command.
With the prefix argument, modify the command's invocation.
NAME is the name of your application.
If BIN is t then create a binary application, otherwise a library.
Cargo: Create a new cargo project.

\(fn NAME &optional BIN)" t nil)

(autoload 'cargo-process-init "cargo-process" "\
Run the Cargo init command.
With the prefix argument, modify the command's invocation.
DIRECTORY is the directory you want to create a cargo project in.
If BIN is t then create a binary application, otherwise a library.
Cargo: Create a new cargo project in current directory.

DIRECTORY is created if necessary.

\(fn DIRECTORY &optional BIN)" t nil)

(autoload 'cargo-process-run "cargo-process" "\
Run the Cargo run command.
With the prefix argument, modify the command's invocation.
Cargo: Build and execute src/main.rs." t nil)

(autoload 'cargo-process-run-bin "cargo-process" "\
Run the Cargo run command --bin <name>.
With the prefix argument, modify the command's invocation.
Cargo: Build and execute a specific binary

\(fn COMMAND)" t nil)

(autoload 'cargo-process-run-example "cargo-process" "\
Run the Cargo run command --example <name>.
With the prefix argument, modify the command's invocation.
Cargo: Build and execute with --example <name>.

\(fn COMMAND)" t nil)

(autoload 'cargo-process-search "cargo-process" "\
Run the Cargo search command.
With the prefix argument, modify the command's invocation.
SEARCH-TERM is used as the search term for the Cargo registry.
Cargo: Search registry for crates.

\(fn SEARCH-TERM)" t nil)

(autoload 'cargo-process-test "cargo-process" "\
Run the Cargo test command.
With the prefix argument, modify the command's invocation.
Cargo: Run the tests." t nil)

(autoload 'cargo-process-current-test "cargo-process" "\
Run the Cargo test command for the current test.
With the prefix argument, modify the command's invocation.
Cargo: Run the tests." t nil)

(autoload 'cargo-process-current-file-tests "cargo-process" "\
Run the Cargo test command for the current file.
With the prefix argument, modify the command's invocation.
Cargo: Run the tests." t nil)

(autoload 'cargo-process-update "cargo-process" "\
Run the Cargo update command.
With the prefix argument, modify the command's invocation.
Cargo: Update dependencies listed in Cargo.lock." t nil)

(autoload 'cargo-process-fmt "cargo-process" "\
Run the Cargo fmt command.
With the prefix argument, modify the command's invocation.
Requires Cargo Fmt to be installed." t nil)

(autoload 'cargo-process-outdated "cargo-process" "\
Run the Cargo outdated command.
With the prefix argument, modify the command's invocation.
Requires Cargo Outdated to be installed." t nil)

(autoload 'cargo-process-check "cargo-process" "\
Run the Cargo check command.
With the prefix argument, modify the command's invocation.
Cargo: Check compile the current project.
Requires cargo-check to be installed." t nil)

(autoload 'cargo-process-clippy "cargo-process" "\
Run the Cargo clippy command.
With the prefix argument, modify the command's invocation.
Cargo: Clippy compile the current project.
Requires Cargo clippy to be installed." t nil)

(autoload 'cargo-process-add "cargo-process" "\
Run the Cargo add command.
With the prefix argument, modify the command's invocation.
CRATES is the name of the crate to add.
Cargo: This command allows you to add a dependency to a Cargo.toml manifest file.

\(fn CRATE)" t nil)

(autoload 'cargo-process-audit "cargo-process" "\
Run the Cargo audit command.
With the prefix argument, modify the command's invocation.
Cargo: Audit checks the current project's Cargo.lock for security vulnerabilities.
Requires Cargo Audit to be installed." t nil)

(autoload 'cargo-process-rm "cargo-process" "\
Run the Cargo rm command.
With the prefix argument, modify the command's invocation.
CRATE is the name of the crate to remove.
Cargo: Remove a dependency from a Cargo.toml manifest file.

\(fn CRATE)" t nil)

(autoload 'cargo-process-upgrade "cargo-process" "\
Run the Cargo update command.
With the prefix argument, modify the command's invocation.
If ALL is t then update all crates, otherwise specify CRATES.
Cargo: Upgrade dependencies as specified in the local manifest file

\(fn &optional ALL CRATES)" t nil)

(autoload 'cargo-process-repeat "cargo-process" "\
Run the last cargo-process command." t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "cargo-process" '("cargo-" "manifest-path-argument" "rustc-errno" "set-rust-backtrace")))

;;;***

;;;### (autoloads nil nil ("cargo-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; cargo-autoloads.el ends here
