;;; go-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "go-guru" "go-guru.el" (22528 24123 0 0))
;;; Generated autoloads from go-guru.el

(autoload 'go-guru-set-scope "go-guru" "\
Set the scope for the Go guru, prompting the user to edit the previous scope.

The scope restricts analysis to the specified packages.
Its value is a comma-separated list of patterns of these forms:
  golang.org/x/tools/cmd/guru     # a single package
  golang.org/x/tools/...          # all packages beneath dir
  ...                             # the entire workspace.

A pattern preceded by '-' is negative, so the scope
  encoding/...,-encoding/xml
matches all encoding packages except encoding/xml.

\(fn)" t nil)

(autoload 'go-guru-callees "go-guru" "\
Show possible callees of the function call at the current point.

\(fn)" t nil)

(autoload 'go-guru-callers "go-guru" "\
Show the set of callers of the function containing the current point.

\(fn)" t nil)

(autoload 'go-guru-callstack "go-guru" "\
Show an arbitrary path from a root of the call graph to the
function containing the current point.

\(fn)" t nil)

(autoload 'go-guru-definition "go-guru" "\
Jump to the definition of the selected identifier.

\(fn)" t nil)

(autoload 'go-guru-describe "go-guru" "\
Describe the selected syntax, its kind, type and methods.

\(fn)" t nil)

(autoload 'go-guru-pointsto "go-guru" "\
Show what the selected expression points to.

\(fn)" t nil)

(autoload 'go-guru-implements "go-guru" "\
Describe the 'implements' relation for types in the package
containing the current point.

\(fn)" t nil)

(autoload 'go-guru-freevars "go-guru" "\
Enumerate the free variables of the current selection.

\(fn)" t nil)

(autoload 'go-guru-peers "go-guru" "\
Enumerate the set of possible corresponding sends/receives for
this channel receive/send operation.

\(fn)" t nil)

(autoload 'go-guru-referrers "go-guru" "\
Enumerate all references to the object denoted by the selected
identifier.

\(fn)" t nil)

(autoload 'go-guru-whicherrs "go-guru" "\
Show globals, constants and types to which the selected
expression (of type 'error') may refer.

\(fn)" t nil)

(autoload 'go-guru-unhighlight-identifiers "go-guru" "\
Remove highlights from previously highlighted identifier.

\(fn)" nil nil)

(autoload 'go-guru-hl-identifier "go-guru" "\
Highlight all instances of the identifier under point. Removes
highlights from previously highlighted identifier.

\(fn)" t nil)

(autoload 'go-guru-hl-identifier-mode "go-guru" "\
Highlight instances of the identifier at point after a short
timeout.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil "go-mode" "go-mode.el" (22528 24123 0 0))
;;; Generated autoloads from go-mode.el

(autoload 'go-mode "go-mode" "\
Major mode for editing Go source text.

This mode provides (not just) basic editing capabilities for
working with Go code. It offers almost complete syntax
highlighting, indentation that is almost identical to gofmt and
proper parsing of the buffer content to allow features such as
navigation by function, manipulation of comments or detection of
strings.

In addition to these core features, it offers various features to
help with writing Go code. You can directly run buffer content
through gofmt, read godoc documentation from within Emacs, modify
and clean up the list of package imports or interact with the
Playground (uploading and downloading pastes).

The following extra functions are defined:

- `gofmt'
- `godoc' and `godoc-at-point'
- `go-import-add'
- `go-remove-unused-imports'
- `go-goto-arguments'
- `go-goto-docstring'
- `go-goto-function'
- `go-goto-function-name'
- `go-goto-imports'
- `go-goto-return-values'
- `go-goto-method-receiver'
- `go-play-buffer' and `go-play-region'
- `go-download-play'
- `godef-describe' and `godef-jump'
- `go-coverage'
- `go-set-project'
- `go-reset-gopath'

If you want to automatically run `gofmt' before saving a file,
add the following hook to your emacs configuration:

\(add-hook 'before-save-hook #'gofmt-before-save)

If you want to use `godef-jump' instead of etags (or similar),
consider binding godef-jump to `M-.', which is the default key
for `find-tag':

\(add-hook 'go-mode-hook (lambda ()
                          (local-set-key (kbd \"M-.\") #'godef-jump)))

Please note that godef is an external dependency. You can install
it with

go get github.com/rogpeppe/godef


If you're looking for even more integration with Go, namely
on-the-fly syntax checking, auto-completion and snippets, it is
recommended that you look at flycheck
\(see URL `https://github.com/flycheck/flycheck') or flymake in combination
with goflymake (see URL `https://github.com/dougm/goflymake'), gocode
\(see URL `https://github.com/nsf/gocode'), go-eldoc
\(see URL `github.com/syohex/emacs-go-eldoc') and yasnippet-go
\(see URL `https://github.com/dominikh/yasnippet-go')

\(fn)" t nil)

(add-to-list 'auto-mode-alist (cons "\\.go\\'" 'go-mode))

(autoload 'gofmt-before-save "go-mode" "\
Add this to .emacs to run gofmt on the current buffer when saving:
 (add-hook 'before-save-hook 'gofmt-before-save).

Note that this will cause go-mode to get loaded the first time
you save any file, kind of defeating the point of autoloading.

\(fn)" t nil)

(autoload 'godoc "go-mode" "\
Show Go documentation for QUERY, much like M-x man.

\(fn QUERY)" t nil)

(autoload 'go-download-play "go-mode" "\
Download a paste from the playground and insert it in a Go buffer.
Tries to look for a URL at point.

\(fn URL)" t nil)

;;;***

;;;### (autoloads nil nil ("go-mode-pkg.el") (22528 24123 0 0))

;;;***

(provide 'go-mode-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; go-mode-autoloads.el ends here
