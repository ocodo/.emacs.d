;;; puppet-mode.el --- Major mode for Puppet manifests  -*- lexical-binding: t; -*-

;; Copyright (C) 2013, 2014  Sebastian Wiesner <lunaryorn@gmail.com>
;; Copyright (C) 2013, 2014  Bozhidar Batsov <bozhidar@batsov.com>
;; Copyright (C) 2011  Puppet Labs Inc

;; Author: Bozhidar Batsov <bozhidar@batsov.com>
;;     Sebastian Wiesner <lunaryorn@gmail.com>
;;     Russ Allbery <rra@stanford.edu>
;; Maintainer: Bozhidar Batsov <bozhidar@batsov.com>
;;     Sebastian Wiesner <lunaryorn@gmail.com>
;; URL: https://github.com/lunaryorn/puppet-mode
;; Keywords: languages
;; Version: 20140316.1452
;; X-Original-Version: 0.4-cvs
;; Package-Requires: ((emacs "24.1") (pkg-info "0.4"))

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; This file incorporates work covered by the following copyright and
;; permission notice:

;;   Licensed under the Apache License, Version 2.0 (the "License"); you may not
;;   use this file except in compliance with the License.  You may obtain a copy
;;   of the License at
;;
;;       http://www.apache.org/licenses/LICENSE-2.0
;;
;;   Unless required by applicable law or agreed to in writing, software
;;   distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
;;   WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the
;;   License for the specific language governing permissions and limitations
;;   under the License.

;;; Commentary:

;; GNU Emacs 24 major mode for editing Puppet manifests.

;; Provides syntax highlighting, indentation, alignment, movement, Imenu and
;; code checking.

;; Syntax highlighting: Fontification upports all of Puppet 3 syntax, including
;; variable expansion in strings.

;; Indentation: Indent expressions automatically.

;; Alignment: Provide alignment rules for common Puppet expressions, and align
;; the current block with `puppet-align-block' on C-c C-a.

;; Movement: Move to the beginning or end of the current block with
;; `beginning-of-defun' (C-M-a) and `end-of-defun' (C-M-e) respectively.

;; Imenu: Jump to a tag in the current buffer with `imenu' on C-c C-j.  Index
;; variables, resource defaults, classes, nodes, defined types and resource
;; declarations.

;; Code checking: Validate the syntax of the current buffer with
;; `puppet-validate' on C-c C-v.  Lint the current buffer for semantic errors
;; with `puppet-lint' on C-c C-l.  Apply the current buffer with `puppet-apply'
;; on C-c C-c.

;; Flymake: Flymake support is _not_ provided. See Flycheck at
;; http://flycheck.readthedocs.org/en/latest/ for on-the-fly validation and
;; liniting of Puppet manifests.

;;; Code:


;;;; Compatibility
(eval-and-compile
  ;; `defvar-local' for Emacs 24.2 and below
  (unless (fboundp 'defvar-local)
    (defmacro defvar-local (var val &optional docstring)
      "Define VAR as a buffer-local variable with default value VAL.
Like `defvar' but additionally marks the variable as being automatically
buffer-local wherever it is set."
      (declare (debug defvar) (doc-string 3))
      `(progn
         (defvar ,var ,val ,docstring)
         (make-variable-buffer-local ',var))))

  ;; `setq-local' for Emacs 24.2 and below
  (unless (fboundp 'setq-local)
    (defmacro setq-local (var val)
      "Set variable VAR to value VAL in current buffer."
      `(set (make-local-variable ',var) ,val))))


;;;; Requirements
(declare-function pkg-info-version-info "pkg-info" (library))

(eval-when-compile
  (require 'rx))

(require 'align)


;;;; Customization
(defgroup puppet nil
  "Puppet mastering in Emacs"
  :prefix "puppet-"
  :group 'languages
  :link '(url-link :tag "Github" "https://github.com/lunaryorn/puppet-mode")
  :link '(emacs-commentary-link :tag "Commentary" "puppet-mode"))

(defcustom puppet-indent-level 2
  "Indentation of Puppet statements."
  :type 'integer
  :group 'puppet
  :safe 'integerp)

(defcustom puppet-include-indent 2
  "Indentation of continued Puppet include statements."
  :type 'integer
  :group 'puppet
  :safe 'integerp)

(defcustom puppet-indent-tabs-mode nil
  "Indentation can insert tabs in puppet mode if this is non-nil."
  :type 'boolean
  :group 'puppet
  :safe 'booleanp)

(defcustom puppet-comment-column 32
  "Indentation column of comments."
  :type 'integer
  :group 'puppet
  :safe 'integerp)

(defcustom puppet-fontify-variables-in-comments nil
  "When non-nil, fontify variable references in comments."
  :type 'boolean
  :group 'puppet
  :safe 'booleanp
  :package-version '(puppet-mode . "0.3"))

(defcustom puppet-validate-command "puppet parser validate --color=false"
  "Command to validate the syntax of a Puppet manifest."
  :type 'string
  :group 'puppet)

(defcustom puppet-lint-command
  (concat
   "puppet-lint --with-context "
   "--log-format \"%{path}:%{linenumber}: %{kind}: %{message} (%{check})\"")
  "Command to lint a Puppet manifest."
  :type 'string
  :group 'puppet
  :package-version '(puppet-mode . "0.3"))

(defcustom puppet-apply-command "puppet apply --verbose --noop"
  "Command to apply a Puppet manifest."
  :type 'string
  :group 'puppet
  :package-version '(puppet-mode . "0.3"))

(defface puppet-regular-expression-literal
  '((t :inherit font-lock-constant-face))
  "Face for regular expression literals in Puppet."
  :group 'puppet
  :package-version '(puppet-mode . "0.3"))

(defface puppet-escape-sequence
  '((t :inherit font-lock-constant-face))
  "Face for escape sequences in double-quoted strings-consed literals in Puppet."
  :group 'puppet
  :package-version '(puppet-mode . "0.3"))


;;;; Version information
(defun puppet-version (&optional show-version)
  "Get the Puppet Mode version as string.

If called interactively or if SHOW-VERSION is non-nil, show the
version in the echo area and the messages buffer.

The returned string includes both, the version from package.el
and the library version, if both a present and different.

If the version number could not be determined, signal an error,
if called interactively, or if SHOW-VERSION is non-nil, otherwise
just return nil."
  (interactive (list t))
  (let ((version (pkg-info-version-info 'puppet-mode)))
    (when show-version
      (message "Puppet Mode version: %s" version))
    version))


;;;; Utilities

(defun puppet-syntax-context (&optional pos)
  "Determine the syntax context at POS, defaulting to point.

Return nil, if there is no special context at POS, or one of

`comment'
     POS is inside a comment

`single-quoted'
     POS is inside a single-quoted string

`double-quoted'
     POS is inside a double-quoted string"
  (let ((state (save-excursion (syntax-ppss pos))))
    (if (nth 4 state)
        'comment
      (pcase (nth 3 state)
        (?\' 'single-quoted)
        (?\" 'double-quoted)))))

(defun puppet-in-string-or-comment-p (&optional pos)
  "Determine whether POS is inside a string or comment."
  (not (null (puppet-syntax-context pos))))


;;;; Specialized rx

(eval-when-compile
  (defconst puppet-rx-constituents
    `(
      ;; http://docs.puppetlabs.com/puppet/latest/reference/lang_reserved.html#reserved-words
      (keyword . ,(rx symbol-start
                      (or "and" "case" "class" "default" "define" "else" "elsif"
                          "false" "if" "in" "import" "inherits" "node" "or"
                          "true" "undef" "unless")
                      symbol-end))
      ;; http://docs.puppetlabs.com/references/latest/function.html
      (builtin-function . ,(rx symbol-start
                               (or "alert" "collect" "contain"
                                   "create_resources" "crit" "debug" "defined"
                                   "each" "emerg" "err" "extlookup" "fail"
                                   "file" "filter" "fqdn_rand" "generate"
                                   "hiera" "hiera_array" "hiera_hash"
                                   "hiera_include" "include" "info"
                                   "inline_template" "lookup" "map" "md5"
                                   "notice" "realize" "reduce" "regsubst"
                                   "require" "search" "select" "sha1"
                                   "shellquote" "slice" "split" "sprintf" "tag"
                                   "tagged" "template" "versioncmp" "warning")
                               symbol-end))
      ;; http://docs.puppetlabs.com/references/latest/type.html
      (builtin-type . ,(rx symbol-start
                           (or "augeas" "computer" "cron" "exec" "file"
                               "filebucket" "group" "host" "interface" "k5login"
                               "macauthorization" "mailalias" "maillist" "mcx"
                               "mount" "nagios_command" "nagios_contact"
                               "nagios_contactgroup" "nagios_host"
                               "nagios_hostdependency" "nagios_hostescalation"
                               "nagios_hostextinfo" "nagios_hostgroup"
                               "nagios_service" "nagios_servicedependency"
                               "nagios_serviceescalation" "nagios_serviceextinfo"
                               "nagios_servicegroup" "nagios_timeperiod" "notify"
                               "package" "resources" "router" "schedule"
                               "scheduled_task" "selboolean" "selmodule"
                               "service" "ssh_authorized_key" "sshkey" "stage"
                               "tidy" "user" "vlan" "yumrepo" "zfs" "zone"
                               "zpool")
                           symbol-end))
      ;; http://docs.puppetlabs.com/references/stable/metaparameter.html.
      ;; Strictly speaking, this is no meta parameter, but it's so common that
      ;; it got a mention in the docs, see
      ;; http://docs.puppetlabs.com/puppet/latest/reference/lang_resources.html#ensure,
      ;; so we'll consider it as metaparameter anyway
      (builtin-metaparam . ,(rx symbol-start
                                (or "alias" "audit" "before" "loglevel" "noop"
                                    "notify" "require" "schedule" "stage"
                                    "subscribe" "tag"
                                    ;; Because it's so common and important
                                    "ensure")
                                symbol-end))
      ;; http://docs.puppetlabs.com/puppet/latest/reference/lang_reserved.html#classes-and-types
      (resource-name . ,(rx
                         ;; Optional top-level scope
                         (optional "::")
                         (zero-or-more symbol-start
                                       (any "a-z")
                                       (zero-or-more (any "a-z" "0-9" "_"))
                                       symbol-end
                                       "::")
                         ;; Nested sub-scopes
                         symbol-start
                         (any "a-z")
                         (zero-or-more (any "a-z" "0-9" "_"))
                         symbol-end))
      (cap-resource-name . ,(rx
                             ;; Top-scope indicator
                             (optional "::")
                             (zero-or-more symbol-start
                                           (any "A-Z")
                                           (zero-or-more
                                            (any "a-z" "0-9" "_"))
                                           symbol-end
                                           "::")
                             ;; Nested sub-scopes
                             symbol-start
                             (any "A-Z")
                             (zero-or-more (any "a-z" "0-9" "_"))
                             symbol-end))
      ;; http://docs.puppetlabs.com/puppet/latest/reference/lang_reserved.html#nodes
      (node-name . ,(rx symbol-start
                        (one-or-more (any "a-z" "0-9" ?. ?_ ?-))
                        symbol-end))
      ;; http://docs.puppetlabs.com/puppet/latest/reference/lang_reserved.html#variables
      (simple-variable-name . ,(rx symbol-start
                                   (one-or-more (any "A-Z" "a-z" "0-9" "_"))
                                   symbol-end))
      (variable-name . ,(rx
                         ;; The optional scope designation
                         (optional "::")
                         (zero-or-more symbol-start
                                       (any "a-z")
                                       (zero-or-more
                                        (any "A-Z" "a-z" "0-9" "_"))
                                       symbol-end
                                       "::")
                         ;; The final variable name
                         symbol-start
                         (one-or-more (any "A-Z" "a-z" "0-9" "_"))
                         symbol-end))
      ;; http://docs.puppetlabs.com/puppet/latest/reference/lang_datatypes.html#double-quoted-strings
      (dq-escape . ,(rx (or line-start (not (any "\\")))
                        (zero-or-more "\\\\")
                        ;; We do not include \n and \', because these are
                        ;; available in single-quoted strings as well
                        (group "\\" (any ?\" ?$ ?n ?r ?t ?s)))))
    "Additional special sexps for `puppet-rx'")

  (defmacro puppet-rx (&rest sexps)
    "Specialized `rx' variant for Puppet Mode.

In addition to the standard forms of `rx', the following forms
are available:

`keyword'
     Any valid Puppet keyword

`builtin-function'
     Any built-in Puppet function

`builtin-type'
     Any built-in Puppet type

`builtin-metaparam'
     Any built-in meta-parameter, and `ensure'

`resource-name'
     Any valid resource name, including scopes

`cap-resource-name'
     Any capitalized resource name, including capitalized scopes

`node-name'
     Any valid node name

`simple-variable-name'
     Any variable name without scopes, without leading dollar sign

`variable-name'
     Any variable name including scopes, without a leading dollar sign

`dq-escape'
     Special escape sequences for double-quoted strings"
    (let ((rx-constituents (append puppet-rx-constituents rx-constituents)))
      (cond ((null sexps)
             (error "No regexp"))
            ((cdr sexps)
             (rx-to-string `(and ,@sexps) t))
            (t
             (rx-to-string (car sexps) t))))))


;;;; Checking

(defvar-local puppet-last-validate-command nil
  "The last command used for validation.")

(defvar-local puppet-last-lint-command nil
  "The last command used for linting.")

;; This variable is intentionally not buffer-local, since you typically only
;; apply top-level manifests, but not class or type definitions.
(defvar puppet-last-apply-command nil
  "The last command used to apply a manifest.")

(defun puppet-run-check-command (command buffer-name-template)
  "Run COMMAND to check the current buffer."
  (save-some-buffers (not compilation-ask-about-save) nil)
  (compilation-start command nil (lambda (_)
                                   (format buffer-name-template command))))

(defun puppet-read-command (prompt previous-command default-command)
  "Read a command from minibuffer with PROMPT."
  (let ((filename (or (buffer-file-name) "")))
    (read-string prompt (or previous-command
                            (concat default-command " "
                                    (shell-quote-argument filename))))))

(defun puppet-validate (command)
  "Validate the syntax of the current buffer with COMMAND.

When called interactively, prompt for COMMAND."
  (interactive (list (puppet-read-command "Validate command: "
                                          puppet-last-validate-command
                                          puppet-validate-command)))
  (setq puppet-last-validate-command command)
  (puppet-run-check-command command "*Puppet Validate: %s*"))

(defun puppet-lint (command)
  "Lint the current buffer with COMMAND.

When called interactively, prompt for COMMAND."
  (interactive (list (puppet-read-command "Lint command: "
                                          puppet-last-lint-command
                                          puppet-lint-command)))
  (setq puppet-last-lint-command command)
  (puppet-run-check-command command "*Puppet Lint: %s*"))

(defun puppet-apply (command)
  "Apply the current manifest with COMMAND.

When called interactively, prompt for COMMAND."
  (interactive (list (puppet-read-command "Apply command: "
                                          puppet-last-apply-command
                                          puppet-apply-command)))
  (setq puppet-last-apply-command command)
  (puppet-run-check-command command "*Puppet Apply: %s*"))


;;;; Navigation
;; TODO: Check which of these are still needed for SMIE

(defun puppet-beginning-of-defun-function (&optional arg)
  "Move to the ARG'th beginning of a block."
  (let* ((arg (or arg 1))
         (search (if (< arg 0) #'search-forward #'search-backward))
         (steps (abs arg)))
    (while (> steps 0)
      (let ((pos (funcall search "{" nil 'no-error)))
        ;; Skip over strings and comments
        (while (and pos (puppet-in-string-or-comment-p pos))
          (setq pos (funcall search "{" nil 'no-error)))
        (if pos
            (setq steps (1- steps))
          ;; Drop out of outer loop
          (setq steps 0))))
    (when (< arg 0)
      (backward-char))))


;;;; Indentation code
(defun puppet-block-indent ()
  "If point is in a block, return the indentation of the first line of that
block (the line containing the opening brace).  Used to set the indentation
of the closing brace of a block."
  (save-excursion
    (beginning-of-defun)
    (current-indentation)))

(defun puppet-in-array ()
  "If point is in an array, return the position of the opening '[' of
that array, else return nil."
  (save-excursion
    (save-match-data
      (let ((opoint (point))
            (apoint (search-backward "[" nil t)))
        (when apoint
          ;; This is a bit of a hack and doesn't allow for strings.  We really
          ;; want to parse by sexps at some point.
          (let ((close-brackets (count-matches "]" apoint opoint))
                (open-brackets 0))
            (while (and apoint (> close-brackets open-brackets))
              (setq apoint (search-backward "[" nil t))
              (when apoint
                (setq close-brackets (count-matches "]" apoint opoint))
                (setq open-brackets (1+ open-brackets)))))
          apoint)))))

(defun puppet-in-include ()
  "If point is in a continued list of include statements, return the position
of the initial include plus puppet-include-indent."
  (save-excursion
    (save-match-data
      (let ((include-column nil)
            (not-found t))
        (while not-found
          (forward-line -1)
          (cond
           ((bobp)
            (setq not-found nil))
           ((looking-at "^\\s-*include\\s-+.*,\\s-*$")
            (setq include-column
                  (+ (current-indentation) puppet-include-indent))
            (setq not-found nil))
           ((not (looking-at ".*,\\s-*$"))
            (setq not-found nil))))
        include-column))))

(defun puppet-indent-line ()
  "Indent current line as puppet code."
  (interactive)
  (beginning-of-line)
  (if (bobp)
      (indent-line-to 0)                ; First line is always non-indented
    (let ((not-indented t)
          (array-start (puppet-in-array))
          (include-start (puppet-in-include))
          (block-indent (puppet-block-indent))
          cur-indent)
      (cond
       (array-start
        ;; This line probably starts with an element from an array.
        ;; Indent the line to the same indentation as the first
        ;; element in that array.  That is, this...
        ;;
        ;;    exec {
        ;;      "add_puppetmaster_mongrel_startup_links":
        ;;      command => "string1",
        ;;      creates => [ "string2", "string3",
        ;;      "string4", "string5",
        ;;      "string6", "string7",
        ;;      "string3" ],
        ;;      refreshonly => true,
        ;;    }
        ;;
        ;; ...should instead look like this:
        ;;
        ;;    exec {
        ;;      "add_puppetmaster_mongrel_startup_links":
        ;;      command => "string1",
        ;;      creates => [ "string2", "string3",
        ;;                   "string4", "string5",
        ;;                   "string6", "string7",
        ;;                   "string8" ],
        ;;      refreshonly => true,
        ;;    }
        (save-excursion
          (goto-char array-start)
          (forward-char 1)
          (re-search-forward "\\S-")
          (forward-char -1)
          (setq cur-indent (current-column))))
       (include-start
        (setq cur-indent include-start))
       ((and (looking-at "^\\s-*},?\\s-*$") block-indent)
        ;; This line contains a closing brace or a closing brace followed by a
        ;; comma and we're at the inner block, so we should indent it matching
        ;; the indentation of the opening brace of the block.
        (setq cur-indent block-indent))
       (t
        ;; Otherwise, we did not start on a block-ending-only line.
        (save-excursion
          ;; Iterate backwards until we find an indentation hint
          (while not-indented
            (forward-line -1)
            (cond
             ;; Comment lines are ignored unless we're at the start of the
             ;; buffer.
             ((eq (puppet-syntax-context) 'comment)
              (if (bobp)
                  (setq not-indented nil)))

             ;; Brace or paren on a line by itself will already be indented to
             ;; the right level, so we can cheat and stop there.
             ((looking-at "^\\s-*[\)}]\\s-*")
              (setq cur-indent (current-indentation))
              (setq not-indented nil))

             ;; Brace (possibly followed by a comma) or paren not on a line by
             ;; itself will be indented one level too much, but don't catch
             ;; cases where the block is started and closed on the same line.
             ((looking-at "^[^\n\({]*[\)}],?\\s-*$")
              (setq cur-indent (- (current-indentation) puppet-indent-level))
              (setq not-indented nil))

             ;; Indent by one level more than the start of our block.  We lose
             ;; if there is more than one block opened and closed on the same
             ;; line but it's still unbalanced; hopefully people don't do that.
             ((looking-at "^.*{[^\n}]*$")
              (setq cur-indent (+ (current-indentation) puppet-indent-level))
              (setq not-indented nil))

             ;; Indent by one level if the line ends with an open paren.
             ((looking-at "^.*\(\\s-*$")
              (setq cur-indent (+ (current-indentation) puppet-indent-level))
              (setq not-indented nil))

             ;; Semicolon ends a block for a resource when multiple resources
             ;; are defined in the same block, but try not to get the case of
             ;; a complete resource on a single line wrong.
             ((looking-at "^\\([^'\":\n]\\|\"[^\n\"]*\"\\|'[^\n']*'\\)*;\\s-*$")
              (setq cur-indent (- (current-indentation) puppet-indent-level))
              (setq not-indented nil))

             ;; Indent an extra level after : since it introduces a resource.
             ((looking-at "^.*:\\s-*$")
              (setq cur-indent (+ (current-indentation) puppet-indent-level))
              (setq not-indented nil))

             ;; Start of buffer.
             ((bobp)
              (setq not-indented nil)))))

        ;; If this line contains only a closing paren, we should lose one
        ;; level of indentation.
        (if (looking-at "^\\s-*\)\\s-*$")
            (setq cur-indent (- cur-indent puppet-indent-level)))))

      ;; We've figured out the indentation, so do it.
      (if (and cur-indent (> cur-indent 0))
          (indent-line-to cur-indent)
        (indent-line-to 0)))))


;;;; Font locking

(defvar puppet-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\' "\"'"  table)
    (modify-syntax-entry ?\" "\"\"" table)
    ;; C-style comments.  Yes, Puppet has these!
    (modify-syntax-entry ?/ ". 14b" table)
    (modify-syntax-entry ?* ". 23b" table)
    (modify-syntax-entry ?#  "<"    table)
    (modify-syntax-entry ?\n ">"    table)
    (modify-syntax-entry ?\\ "\\"   table)
    (modify-syntax-entry ?$  "'"    table)
    (modify-syntax-entry ?-  "."    table)
    (modify-syntax-entry ?\( "()"   table)
    (modify-syntax-entry ?\) ")("   table)
    (modify-syntax-entry ?\{ "(}"   table)
    (modify-syntax-entry ?\} "){"   table)
    (modify-syntax-entry ?\[ "(]"   table)
    (modify-syntax-entry ?\] ")["   table)
    table)
  "Syntax table in use in `puppet-mode' buffers.")

(defvar puppet-font-lock-keywords
  `(
    ;; Regular expression literals
    (, (rx (group "/"
                  (zero-or-more
                   (or (not (any "/" "\\" "\n")) ; Not the end of a regexp
                       (and "\\" not-newline)))  ; Any escaped character
                  "/")) 1 'puppet-regular-expression-literal)
    ;; Keywords
    (,(puppet-rx keyword) 0 font-lock-keyword-face)
    ;; Variables
    (,(puppet-rx "$" variable-name) 0 font-lock-variable-name-face)
    ;; Class and type declarations
    (,(puppet-rx symbol-start (or "class" "define") symbol-end
                 (one-or-more space)
                 (group resource-name))
     1 font-lock-type-face)
    ;; Node declarations
    (,(puppet-rx symbol-start "node" symbol-end
                 (one-or-more space)
                 (group node-name))
     1 font-lock-type-face)
    ;; Resource usage, see
    ;; http://docs.puppetlabs.com/puppet/latest/reference/lang_resources.html
    (,(puppet-rx symbol-start
                 (group (repeat 0 2 "@") ; Virtual and exported resources
                        resource-name)
                 (zero-or-more space) "{")
     1 font-lock-type-face)
    ;; Resource defaults, see
    ;; http://docs.puppetlabs.com/puppet/latest/reference/lang_defaults.html
    (,(puppet-rx (group cap-resource-name) (zero-or-more space) "{")
     1 font-lock-type-face)
    ;; Resource references, see
    ;; http://docs.puppetlabs.com/puppet/latest/reference/lang_datatypes.html#resource-references
    (,(puppet-rx (group cap-resource-name) (zero-or-more space) "[")
     1 font-lock-type-face)
    ;; Resource collectors, see
    ;; http://docs.puppetlabs.com/puppet/latest/reference/lang_collectors.html
    (,(puppet-rx (group cap-resource-name) (zero-or-more space)
                 (optional "<")         ; Exported collector
                 "<|")
     1 font-lock-type-face)
    ;; Negation
    ("!" 0 font-lock-negation-char-face)
    ;; Builtin meta parameters
    (,(puppet-rx (group builtin-metaparam) (zero-or-more space) "=>")
     1 font-lock-builtin-face)
    ;; Built-in functions
    (,(puppet-rx builtin-function) 0 font-lock-builtin-face)
    ;; Variable expansions in strings and comments
    (puppet-match-valid-expansion 1 font-lock-variable-name-face t)
    (puppet-match-invalid-expansion 1 font-lock-warning-face t)
    ;; Escape sequences in strings
    (puppet-match-valid-escape 1 'puppet-escape-sequence t)
    )
  "Font lock keywords for Puppet Mode.")

(defun puppet-match-property (property context limit)
  "Match a PROPERTY in CONTEXT before LIMIT.

PROPERTY is the text property to look for.  CONTEXT is one of
`single-quoted', `double-quoted' or `comment', or a list with any
of these symbols.  The expansion will only match if it is in any
given CONTEXT."
  (when (symbolp context)
    (setq context (list context)))
  (let* ((pos (next-single-char-property-change (point) property nil limit)))
    (when (and pos (> pos (point)))
      (goto-char pos)
      (let* ((value (get-text-property pos property)))
        (if (memq (car value) context)
            (progn (set-match-data (cdr value)) t)
          (puppet-match-property property context limit))))))

(defun puppet-match-valid-expansion (limit)
  "Match a valid expansion before LIMIT.

A valid expansion is a variable expansion in a double-quoted
string."
  (let ((valid-contexts '(double-quoted)))
    (when puppet-fontify-variables-in-comments
      (push 'comment valid-contexts))
    (puppet-match-property 'puppet-expansion valid-contexts limit)))

(defun puppet-match-invalid-expansion (limit)
  "Match an invalid expansion before LIMIT.

An invalid expansion is a variable expansion in a single-quoted
string."
  (puppet-match-property 'puppet-expansion 'single-quoted limit))

(defun puppet-match-valid-escape (limit)
  "Match a valid escape sequence before LIMIT."
  (puppet-match-property 'puppet-escape 'double-quoted limit))

(defun puppet-syntax-propertize-match (property)
  "Propertize a match with PROPERTY.

When in a special syntax context, add PROPERTY to the first
character of the first group of the current `match-data'.  The
value of PROPERTY is `(CONTEXT . MATCH-DATA)', where CONTEXT is
one of nil, `single-quoted', `double-quoted' or `comment' and
denotes the surrounding context, and MATCH-DATA is the original
match data from propertization."
  (let* ((beg (match-beginning 1))
         (context (puppet-syntax-context)))
    (when context
      (put-text-property beg (1+ beg) property
                         (cons context (match-data))))))

(defun puppet-syntax-propertize-function (start end)
  "Propertize text between START and END.

Used as `syntax-propertize-function' in Puppet Mode."
  (let ((case-fold-search nil))
    (goto-char start)
    (remove-text-properties start end '(puppet-expansion puppet-escape))
    (funcall
     (syntax-propertize-rules
      ;; Find escape sequences and variable expansions
      ((puppet-rx dq-escape)
       (1 (ignore (puppet-syntax-propertize-match 'puppet-escape))))
      ((puppet-rx (or line-start (not (any "\\")))
                  (zero-or-more "\\\\")
                  (group "$" (or (and "{" variable-name "}") variable-name)))
       (1 (ignore (puppet-syntax-propertize-match 'puppet-expansion)))))
     start end)))


;;;; Alignment

;; Configure alignment
(add-to-list 'align-sq-string-modes 'puppet-mode)
(add-to-list 'align-dq-string-modes 'puppet-mode)
(add-to-list 'align-open-comment-modes 'puppet-mode)

(defconst puppet-mode-align-rules
  '((puppet-resource-arrow
     (regexp . "\\(\\s-*\\)=>\\(\\s-*\\)")
     (group  . (1 2))
     (modes  . '(puppet-mode))))
  "Align rules for Puppet Mode.")

(defun puppet-align-block ()
  "Align the current block."
  (interactive)
  (save-excursion
    (beginning-of-defun)
    (let ((beg (point)))
      (end-of-defun)
      (align beg (point)))))


;;;; Imenu

(defun puppet-imenu-collect-entries (pattern)
  "Collect all index entries matching PATTERN.

The first matching group of PATTERN is used as title and position
for each entry."
  (goto-char (point-min))
  (let ((case-fold-search nil)
        entries)
    (while (re-search-forward pattern nil 'no-error)
      (let ((entry (cons (match-string 1) (match-beginning 1))))
        (unless (puppet-in-string-or-comment-p (match-beginning 0))
          ;; Skip this match if it's inside a string or comment
          (push entry entries))))
    (nreverse entries)))

(defun puppet-imenu-create-index ()
  "Create an IMenu index for the current buffer."
  (let ((case-fold-search nil)
        ;; Variable assignments
        (variables (puppet-imenu-collect-entries
                    (puppet-rx (group "$" simple-variable-name)
                               (zero-or-more space) "=")))
        ;; Resource defaults
        (defaults (puppet-imenu-collect-entries
                   (puppet-rx (group cap-resource-name)
                              (zero-or-more space) "{")))
        ;; Nodes, classes and defines
        (nodes (puppet-imenu-collect-entries
                (puppet-rx symbol-start "node" symbol-end
                           (one-or-more space) (group node-name))))
        (classes (puppet-imenu-collect-entries
                  (puppet-rx symbol-start "class" symbol-end
                             (one-or-more space) (group resource-name))))
        (defines (puppet-imenu-collect-entries
                  (puppet-rx symbol-start "define" symbol-end
                             (one-or-more space) (group resource-name))))
        resources)
    ;; Resources are a little more complicated since we need to extract the type
    ;; and the name
    (goto-char (point-min))
    (while (re-search-forward
            (puppet-rx symbol-start
                       (group (repeat 0 2 "@") ; Virtual and exported resources
                              resource-name)
                       (zero-or-more space) "{"
                       ;; FIXME: Support condensed forms
                       (zero-or-more space)
                       (group (one-or-more not-newline)) ":")
            nil 'no-error)
      ;; FIXME: Doesn't work for any condensed forms, see
      ;; http://docs.puppetlabs.com/puppet/latest/reference/lang_resources.html#condensed-forms
      ;; We probably need to be more clever here
      (push (cons (concat (match-string 1) " " (match-string 2))
                  (match-beginning 1))
            resources))
    (let (index
          ;; Keep this in reversed order, for `push'
          (parts (list (cons "Variables" variables)
                       (cons "Defaults" defaults)
                       (cons "Definitions" defines)
                       (cons "Classes" classes)
                       (cons "Nodes" nodes))))
      (dolist (part parts)
        (when (cdr part)
          (push part index)))
      (append index (nreverse resources)))))


;;;; Major mode definition

(defvar puppet-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Editing
    (define-key map (kbd "C-c C-a") #'puppet-align-block)
    ;; Navigation
    (define-key map (kbd "C-c C-j") #'imenu)
    ;; Apply manifests
    (define-key map (kbd "C-c C-c") #'puppet-apply)
    ;; Linting and validation
    (define-key map (kbd "C-c C-v") #'puppet-validate)
    (define-key map (kbd "C-c C-l") #'puppet-lint)
    ;; The menu bar
    (easy-menu-define puppet-menu map "Puppet Mode menu"
      `("Puppet"
        :help "Puppet-specific Features"
        ["Align the current block" puppet-align-block
         :help "Align parameters in the current block"]
        "-"
        ["Jump to resource/variable" imenu
         :help "Jump to a resource or variable"]
        "-"
        ["Apply manifest" puppet-apply :help "Apply a Puppet manifest"]
        "-"
        ["Validate file syntax" puppet-validate
         :help "Validate the syntax of this file"]
        ["Lint file" puppet-lint
         :help "Check the file for semantic issues"]))
    map)
  "Key map for Puppet Mode buffers.")

;;;###autoload
(define-derived-mode puppet-mode prog-mode "Puppet" ()
  "Major mode for editing Puppet manifests.

\\{puppet-mode-map}"
  ;; Misc variables
  (setq-local require-final-newline t)
  ;; Comment setup
  (setq-local comment-start "# ")
  (setq-local comment-start-skip "#+ *")
  (setq-local comment-use-syntax t)
  (setq-local comment-end "")
  (setq-local comment-auto-fill-only-comments t)
  (setq comment-column puppet-comment-column)
  ;; Navigation (TODO: Will we still need this with SMIE?)
  (setq-local beginning-of-defun-function #'puppet-beginning-of-defun-function)
  ;; Indentation
  (setq-local indent-line-function #'puppet-indent-line)
  (setq indent-tabs-mode puppet-indent-tabs-mode)
  ;; Paragaphs
  (setq-local paragraph-ignore-fill-prefix t)
  (setq-local paragraph-start "\f\\|[ \t]*$\\|#$")
  (setq-local paragraph-separate "\\([ \t\f]*\\|#\\)$")
  ;; Font locking
  (setq font-lock-defaults '((puppet-font-lock-keywords) nil nil))
  (setq-local syntax-propertize-function #'puppet-syntax-propertize-function)
  ;; Alignment
  (setq align-mode-rules-list puppet-mode-align-rules)
  ;; IMenu
  (setq imenu-create-index-function #'puppet-imenu-create-index))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.pp\\'" . puppet-mode))

(provide 'puppet-mode)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; puppet-mode.el ends here
