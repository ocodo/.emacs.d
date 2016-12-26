;;; context-coloring-javascript.el --- JavaScript support  -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2016  Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

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

;; Add JavaScript context coloring support with js2-mode.

;;; Code:

(require 'context-coloring)
(require 'js2-mode)


;;; JavaScript colorization

(defvar-local context-coloring-js2-scope-level-hash-table nil
  "Associate `js2-scope' structures and with their scope
  levels.")

(defcustom context-coloring-javascript-block-scopes nil
  "If non-nil, also color block scopes in the scope hierarchy in JavaScript.

The block-scoped `let' and `const' are introduced in ES6.  Enable
this for ES6 code; disable it elsewhere."
  :type 'boolean
  :safe #'booleanp
  :group 'context-coloring)

(defsubst context-coloring-js2-scope-level (scope initial)
  "Return the level of SCOPE, starting from INITIAL."
  (cond ((gethash scope context-coloring-js2-scope-level-hash-table))
        (t
         (let ((level initial)
               (current-scope scope)
               enclosing-scope)
           (while (and current-scope
                       (js2-node-parent current-scope)
                       (setq enclosing-scope
                             (js2-node-get-enclosing-scope current-scope)))
             (when (or context-coloring-javascript-block-scopes
                       (let ((type (js2-scope-type current-scope)))
                         (or (= type js2-SCRIPT)
                             (= type js2-FUNCTION)
                             (= type js2-CATCH))))
               (setq level (+ level 1)))
             (setq current-scope enclosing-scope))
           (puthash scope level context-coloring-js2-scope-level-hash-table)))))

(defsubst context-coloring-js2-local-name-node-p (node)
  "Determine if NODE represents a local variable."
  (and (js2-name-node-p node)
       (let ((parent (js2-node-parent node)))
         (not (or (and (js2-object-prop-node-p parent)
                       (eq node (js2-object-prop-node-left parent)))
                  (and (js2-prop-get-node-p parent)
                       ;; For nested property lookup, the node on the left is a
                       ;; `js2-prop-get-node', so this always works.
                       (eq node (js2-prop-get-node-right parent))))))))

(defvar-local context-coloring-point-min nil
  "Cached value of `point-min'.")

(defvar-local context-coloring-point-max nil
  "Cached value of `point-max'.")

(defsubst context-coloring-js2-bounded-point (point)
  "Make POINT safe to set text properties.
POINT may be unsafe if a JS2 node extends beyond the end of the
buffer (in the case of an unterminated multiline comment).  The
region could also be narrowed and the node thus obscured."
  (min (max point context-coloring-point-min) context-coloring-point-max))

(defsubst context-coloring-js2-colorize-node (node level)
  "Color NODE with the color for LEVEL."
  (let* ((start (js2-node-abs-pos node))
         (end (+ start (js2-node-len node))))
    (context-coloring-colorize-region
     (context-coloring-js2-bounded-point start)
     (context-coloring-js2-bounded-point end)
     level)))

(defun context-coloring-js2-colorize-ast ()
  "Color the buffer using the `js2-mode' abstract syntax tree."
  ;; Reset the hash table; the old one could be obsolete.
  (setq context-coloring-js2-scope-level-hash-table (make-hash-table :test #'eq))
  (setq context-coloring-point-min (point-min))
  (setq context-coloring-point-max (point-max))
  (with-silent-modifications
    (js2-visit-ast
     js2-mode-ast
     (lambda (node end-p)
       (when (null end-p)
         (cond
          ((js2-scope-p node)
           (context-coloring-js2-colorize-node
            node
            (context-coloring-js2-scope-level node context-coloring-initial-level)))
          ((context-coloring-js2-local-name-node-p node)
           (let* ((enclosing-scope (js2-node-get-enclosing-scope node))
                  (defining-scope (js2-get-defining-scope
                                   enclosing-scope
                                   (js2-name-node-name node))))
             ;; The tree seems to be walked lexically, so an entire scope will
             ;; be colored, including its name nodes, before they are reached.
             ;; Coloring the nodes defined in that scope would be redundant, so
             ;; don't do it.
             (when (not (eq defining-scope enclosing-scope))
               (context-coloring-js2-colorize-node
                node
                ;; Use `0' as an initial level so global variables are always at
                ;; the highest level (even if `context-coloring-initial-level'
                ;; specifies an initial level for the rest of the code).
                (context-coloring-js2-scope-level defining-scope 0))))))
         ;; The `t' indicates to search children.
         t)))
    (context-coloring-colorize-comments-and-strings)))

(defconst context-coloring-node-comment-regexp
  (concat
   ;; Ensure the "//" or "/*" comment starts with the directive.
   "\\(//[[:space:]]*\\|/\\*[[:space:]]*\\)"
   ;; Support multiple directive formats.
   "\\("
   ;; JSLint and JSHint support a JSON-like format.
   "\\(jslint\\|jshint\\)[[:space:]].*?node:[[:space:]]*true"
   "\\|"
   ;; ESLint just specifies the option name.
   "eslint-env[[:space:]].*?node"
   "\\)")
  "Match a comment body hinting at a Node.js program.")

(defun context-coloring-js2-top-level-local-p ()
  "Guess whether top-level variables are local.
For instance, the current file could be a Node.js program."
  (or
   ;; A shebang is a pretty obvious giveaway.
   (string-equal
    "node"
    (save-excursion
      (goto-char (point-min))
      (when (looking-at auto-mode-interpreter-regexp)
        (match-string 2))))
   ;; Otherwise, perform static analysis.
   (progn
     (setq context-coloring-js2-scope-level-hash-table (make-hash-table :test #'eq))
     (catch 'node-program-p
       (js2-visit-ast
        js2-mode-ast
        (lambda (node end-p)
          (when (null end-p)
            (when
                (cond
                 ;; Infer based on inline linter configuration.
                 ((js2-comment-node-p node)
                  (string-match-p
                   context-coloring-node-comment-regexp
                   (js2-node-string node)))
                 ;; Infer based on the prescence of certain variables.
                 ((and (js2-name-node-p node)
                       (let ((parent (js2-node-parent node)))
                         (not (and (js2-object-prop-node-p parent)
                                   (eq node (js2-object-prop-node-left parent))))))
                  (let ((name (js2-name-node-name node))
                        (parent (js2-node-parent node)))
                    (and
                     (cond
                      ;; Check whether this is "exports.something" or
                      ;; "module.exports".
                      ((js2-prop-get-node-p parent)
                       (and
                        (eq node (js2-prop-get-node-left parent))
                        (or (string-equal name "exports")
                            (let* ((property (js2-prop-get-node-right parent))
                                   (property-name (js2-name-node-name property)))
                              (and (string-equal name "module")
                                   (string-equal property-name "exports"))))))
                      ;; Check whether it's a "require('module')" call.
                      ((js2-call-node-p parent)
                       (or (string-equal name "require"))))
                     (let* ((enclosing-scope (js2-node-get-enclosing-scope node))
                            (defining-scope (js2-get-defining-scope
                                             enclosing-scope name)))
                       ;; The variable also must be global.
                       (null defining-scope))))))
              (throw 'node-program-p t))
            ;; The `t' indicates to search children.
            t)))
       ;; Default to returning nil from the catch body.
       nil))))

(defcustom context-coloring-javascript-detect-top-level-scope t
  "If non-nil, detect when to use file-level scope."
  :type 'boolean
  :group 'context-coloring)

;;;###autoload
(defun context-coloring-js2-colorize ()
  "Color the buffer using the `js2-mode'."
  (cond
   ;; Increase the initial level if we should.
   ((and context-coloring-javascript-detect-top-level-scope
         (context-coloring-js2-top-level-local-p))
    (let ((context-coloring-initial-level 1))
      (context-coloring-js2-colorize-ast)))
   (t
    (context-coloring-js2-colorize-ast))))

;;;###autoload
(puthash
 'javascript
 (list :modes '(js2-mode js2-jsx-mode)
       :colorizer #'context-coloring-js2-colorize
       :setup
       (lambda ()
         (add-hook 'js2-post-parse-callbacks #'context-coloring-colorize nil t))
       :teardown
       (lambda ()
         (remove-hook 'js2-post-parse-callbacks #'context-coloring-colorize t)))
 context-coloring-dispatch-hash-table)

(provide 'context-coloring-javascript)

;;; context-coloring-javascript.el ends here
