GNU Emacs 24 major mode for editing Puppet manifests.

Provides syntax highlighting, indentation, alignment, movement, Imenu and
code checking.

Syntax highlighting: Fontification upports all of Puppet 3 syntax, including
variable expansion in strings.

Indentation: Indent expressions automatically.

Alignment: Provide alignment rules for common Puppet expressions, and align
the current block with `puppet-align-block' on C-c C-a.

Movement: Move to the beginning or end of the current block with
`beginning-of-defun' (C-M-a) and `end-of-defun' (C-M-e) respectively.

Imenu: Jump to a tag in the current buffer with `imenu' on C-c C-j.  Index
variables, resource defaults, classes, nodes, defined types and resource
declarations.

Code checking: Validate the syntax of the current buffer with
`puppet-validate' on C-c C-v.  Lint the current buffer for semantic errors
with `puppet-lint' on C-c C-l.  Apply the current buffer with `puppet-apply'
on C-c C-c.

Flymake: Flymake support is _not_ provided. See Flycheck at
http://flycheck.readthedocs.org/en/latest/ for on-the-fly validation and
liniting of Puppet manifests.
