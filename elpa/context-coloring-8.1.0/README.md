# Context Coloring [![Build Status](https://travis-ci.org/jacksonrayhamilton/context-coloring.png?branch=master)](https://travis-ci.org/jacksonrayhamilton/context-coloring) [![Coverage Status](https://coveralls.io/repos/jacksonrayhamilton/context-coloring/badge.svg?branch=master)](https://coveralls.io/r/jacksonrayhamilton/context-coloring?branch=master)

<p align="center">
  <img alt="Screenshot of JavaScript code highlighted by context." src="screenshot.png" title="Screenshot">
</p>

Highlights code by scope.  Top-level scopes are one color, second-level scopes
are another color, and so on.  Variables retain the color of the scope in which
they are defined.  A variable defined in an outer scope referenced by an inner
scope is colored the same as the outer scope.

By default, comments and strings are still highlighted syntactically.

## Features

- Light and dark customizable color schemes.
- JavaScript support:
  - Script, function and block scopes (and even `catch` block scopes).
  - Node.js "file-level" scope detection.
- Emacs Lisp support:
  - `defun`, `lambda`, `let`, `let*`, `cond`, `condition-case`, `defadvice`,
    `dolist`, `quote`, `backquote` and backquote splicing.
  - Works in `eval-expression` too.

## Installation

Requires Emacs 24.3+.  JavaScript language support requires
[js2-mode](https://github.com/mooz/js2-mode) (or, if you use
[Tern](http://ternjs.net/), you may be interested in
[tern-context-coloring](https://github.com/jacksonrayhamilton/tern-context-coloring)).

To install, run the command `M-x package-install RET context-coloring RET`, and
then add the following to your init file:

```lisp
;; JavaScript:
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-hook 'js2-mode-hook #'context-coloring-mode)

;; Emacs Lisp:
(add-hook 'emacs-lisp-mode-hook #'context-coloring-mode)

;; eval-expression:
(add-hook 'eval-expression-minibuffer-setup-hook #'context-coloring-mode) ; 24.4+
(add-hook 'minibuffer-setup-hook #'context-coloring-mode)                 ; 24.3
```

## Color Schemes

The [Zenburn](https://github.com/bbatsov/zenburn-emacs) theme, featured in the
screenshot above, now supports context coloring.

You can define your own colors by customizing faces like
`context-coloring-level-N-face`, where N is a number starting from 0.

[See here](https://gist.github.com/jacksonrayhamilton/6b89ca3b85182c490816) for
some color schemes for popular custom themes.

## Options

- `context-coloring-syntactic-comments` (default: `t`): If non-nil, also color
  comments using `font-lock`.
- `context-coloring-syntactic-strings` (default: `t`): If non-nil, also color
  strings using `font-lock`.
- `context-coloring-javascript-block-scopes` (default: `nil`): If non-nil, also
  color block scopes in the scope hierarchy in JavaScript.
- `context-coloring-javascript-detect-top-level-scope` (default: `t`): If
  non-nil, detect when to use file-level scope.
