;;; puppet-mode.el --- Major mode for Puppet manifests

;; Copyright (C) 2013  Sebastian Wiesner <lunaryorn@gmail.com>
;; Copyright (C) 2013  Bozhidar Batsov <bozhidar@batsov.com>
;; Copyright (C) 2011  Puppet Labs Inc

;; Author: Russ Allbery <rra@stanford.edu>
;; Maintainer: Sebastian Wiesner <lunaryorn@gmail.com>
;; Maintainer: Bozhidar Batsov <bozhidar@batsov.com>
;; URL: https://github.com/lunaryorn/puppet-mode
;; Keywords: languages
;; Version: 20140215.55
;; X-Original-Version: 0.2
;; Package-Requires:

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

;; Major mode for Puppet manifests

;;; Code:


;;;; Compatibility
(eval-and-compile
  ;; `setq-local' for Emacs 24.2 and below
  (unless (fboundp 'setq-local)
    (defmacro setq-local (var val)
      "Set variable VAR to value VAL in current buffer."
      `(set (make-local-variable ',var) ,val))))


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


;;;; Indentation code
(defun puppet-comment-line-p ()
  "Return non-nil iff this line is a comment."
  (save-excursion
    (save-match-data
      (beginning-of-line)
      (looking-at (format "\\s-*%s" comment-start)))))

(defun puppet-block-indent ()
  "If point is in a block, return the indentation of the first line of that
block (the line containing the opening brace).  Used to set the indentation
of the closing brace of a block."
  (save-excursion
    (save-match-data
      (let ((opoint (point))
            (apoint (search-backward "{" nil t)))
        (when apoint
          ;; This is a bit of a hack and doesn't allow for strings.  We really
          ;; want to parse by sexps at some point.
          (let ((close-braces (count-matches "}" apoint opoint))
                (open-braces 0))
            (while (and apoint (> close-braces open-braces))
              (setq apoint (search-backward "{" nil t))
              (when apoint
                (setq close-braces (count-matches "}" apoint opoint))
                (setq open-braces (1+ open-braces)))))
          (if apoint
              (current-indentation)
            nil))))))

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
             ((puppet-comment-line-p)
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


;;;; Major mode definition

(defvar puppet-mode-map (make-sparse-keymap)
  "Key map used in puppet-mode buffers.")

(defvar puppet-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\' "\"'"  table)
    (modify-syntax-entry ?\" "\"\"" table)
    (modify-syntax-entry ?#  "<"    table)
    (modify-syntax-entry ?\n ">#"   table)
    (modify-syntax-entry ?\\ "\\"   table)
    (modify-syntax-entry ?$  "'"    table)
    (modify-syntax-entry ?-  "_"    table)
    (modify-syntax-entry ?:  "_"    table)
    (modify-syntax-entry ?>  "."    table)
    (modify-syntax-entry ?=  "."    table)
    (modify-syntax-entry ?\; "."    table)
    (modify-syntax-entry ?\( "()"   table)
    (modify-syntax-entry ?\) ")("   table)
    (modify-syntax-entry ?\{ "(}"   table)
    (modify-syntax-entry ?\} "){"   table)
    (modify-syntax-entry ?\[ "(]"   table)
    (modify-syntax-entry ?\] ")["   table)
    table)
  "Syntax table in use in puppet-mode buffers.")

(defvar puppet-font-lock-syntax-table
  (let* ((tbl (copy-syntax-table puppet-mode-syntax-table)))
    (modify-syntax-entry ?_ "w" tbl)
    tbl))

(defvar puppet-font-lock-keywords
  (list
   ;; defines, classes, and nodes
   '("^\\s *\\(class\\|define\\|node\\)\\s +\\([^( \t\n]+\\)"
     2 font-lock-function-name-face)
   ;; inheritence
   '("\\s +inherits\\s +\\([^( \t\n]+\\)"
     1 font-lock-function-name-face)
   ;; include
   '("\\(^\\|\\s +\\)include\\s +\\(\\([a-zA-Z0-9:_-]+\\(,[ \t\n]*\\)?\\)+\\)"
     2 font-lock-reference-face)
   ;; keywords
   (cons (regexp-opt
          '("alert"
            "case"
            "class"
            "create_resources"
            "crit"
            "debug"
            "default"
            "define"
            "defined"
            "else"
            "emerg"
            "err"
            "extlookup"
            "fail"
            "false"
            "file"
            "filebucket"
            "fqdn_rand"
            "generate"
            "hiera"
            "hiera_array"
            "hiera_hash"
            "hiera_include"
            "if"
            "import"
            "include"
            "info"
            "inherits"
            "inline_template"
            "md5"
            "node"
            "notice"
            "realize"
            "regsubst"
            "require"
            "search"
            "sha1"
            "shellquote"
            "split"
            "sprintf"
            "tag"
            "tagged"
            "template"
            "true"
            "unless"
            "versioncmp"
            "warning")
          'words)
         1)
     ;; variables
     '("\\(^\\|[^_:.@$]\\)\\b\\(true\\|false\\)\\>"
       2 font-lock-variable-name-face)
     '("\\$[a-zA-Z0-9_:]+"
       0 font-lock-variable-name-face)
     ;; usage of types
     '("^\\s *\\([a-z][a-zA-Z0-9_:-]*\\)\\s +{"
       1 font-lock-type-face)
     ;; overrides and type references
     '("\\s +\\([A-Z][a-zA-Z0-9_:-]*\\)\\["
       1 font-lock-type-face)
     ;; general delimited string
     '("\\(^\\|[[ \t\n<+(,=]\\)\\(%[xrqQwW]?\\([^<[{(a-zA-Z0-9 \n]\\)[^\n\\\\]*\\(\\\\.[^\n\\\\]*\\)*\\(\\3\\)\\)"
       (2 font-lock-string-face)))
  "*Additional expressions to highlight in puppet mode.")

;;;###autoload
(define-derived-mode puppet-mode prog-mode "Puppet" ()
  "Major mode for editing Puppet manifests.

\\{puppet-mode-map}"
  (setq-local comment-start "# ")
  (setq-local comment-start-skip "#+ *")
  (setq-local comment-use-syntax t)
  (setq-local comment-end "")
  (setq-local comment-auto-fill-only-comments t)
  (setq-local comment-column puppet-comment-column)
  (setq-local indent-line-function 'puppet-indent-line)
  (setq-local indent-tabs-mode puppet-indent-tabs-mode)
  (setq-local require-final-newline t)
  (setq-local paragraph-ignore-fill-prefix t)
  (setq-local paragraph-start "\f\\|[ 	]*$\\|#$")
  (setq-local paragraph-separate "\\([ 	\f]*\\|#\\)$")
  (setq-local font-lock-keywords puppet-font-lock-keywords)
  (setq-local font-lock-multiline t)
  (setq-local font-lock-defaults
       '((puppet-font-lock-keywords) nil nil)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.pp\\'" . puppet-mode))

(provide 'puppet-mode)

;;; puppet-mode.el ends here
