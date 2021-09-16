;;; format-all-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "format-all" "format-all.el" (0 0 0 0))
;;; Generated autoloads from format-all.el

(autoload 'format-all-buffer "format-all" "\
Auto-format the source code in the current buffer.

No disk files are touched - the buffer doesn't even need to be
saved.  If you don't like the results of the formatting, you can
use ordinary undo to get your code back to its previous state.

You will need to install external programs to do the formatting.
If the command can't find the program that it needs, it will try
to tell you how you might be able to install it on your operating
system. Only BibTeX, Emacs Lisp and Ledger are formatted without an
external program.

A suitable formatter is selected according to the `major-mode' of
the buffer.  Many popular programming languages are supported.
It is fairly easy to add new languages that have an external
formatter.  When called interactively or PROMPT-P is non-nil, a
missing formatter is prompted in the minibuffer.

If PROMPT is non-nil (or the function is called as an interactive
command), a missing formatter is prompted in the minibuffer.  If
PROMPT is the symbol `always' (or a prefix argument is given),
the formatter is prompted for even if one has already been set.

If any errors or warnings were encountered during formatting,
they are shown in a buffer called *format-all-errors*.

\(fn &optional PROMPT)" t nil)

(autoload 'format-all-region "format-all" "\
Auto-format the source code in the current region.

Like `format-all-buffer' but format only the active region
instead of the entire buffer.  This requires support from the
formatter.

Called non-interactively, START and END delimit the region.
The PROMPT argument works as for `format-all-buffer'.

\(fn START END &optional PROMPT)" t nil)

(autoload 'format-all-mode "format-all" "\
Toggle automatic source code formatting before save.

When this minor mode (FmtAll) is enabled, `format-all-buffer' is
automatically called to format your code each time before you
save the buffer.

The mode is buffer-local and needs to be enabled separately each
time a file is visited. You may want to use `add-hook' in your
`user-init-file' to enable the mode based on buffer modes. E.g.:

    (add-hook 'prog-mode-hook 'format-all-mode)

To use a default formatter for projects that don't have one, add
this too:

    (add-hook 'prog-mode-hook 'format-all-ensure-formatter)

When `format-all-mode' is called as a Lisp function, the mode is
toggled if ARG is ‘toggle’, disabled if ARG is a negative integer
or zero, and enabled otherwise.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "format-all" '("atsfmt" "auctex" "beautysh" "black" "brittany" "bsrefmt" "buildifier" "cabal-fmt" "cmake-format" "crystal" "dartfmt" "define-format-all-formatter" "dfmt" "dhall" "dockfmt" "elm-format" "emacs-" "fantomas" "fish-indent" "fprettify" "gawk" "gleam" "hindent" "html-tidy" "istyle-verilog" "jsonnetfmt" "ktlint" "latexindent" "ledger-mode" "lua-fmt" "mix-format" "nix" "ocp-indent" "ormolu" "perltidy" "pgformatter" "prettier" "pur" "rescript" "scalafmt" "shfmt" "snakefmt" "sqlformat" "swiftformat" "terraform-fmt" "v-fmt" "yapf")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; format-all-autoloads.el ends here
