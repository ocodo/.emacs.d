;;; md-readme.el --- Markdown-formatted READMEs for your ELisp

;; Copyright (C) 2009 Thomas Kappler

;; Author: Thomas Kappler <tkappler@gmail.com>
;; Created: 2009 November 07
;; Keywords: lisp, help, readme, markdown, header, documentation, github
;; URL: <http://github.com/thomas11/md-readme/tree/master>

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

;;; Commentary:

;; The git-based source code hosting site <http://github.com> has
;; lately become popular for Emacs Lisp projects. Github has a feature
;; that displays files named "README[.suffix]" automatically on a
;; project's main page. If these files are formatted in Markdown, the
;; formatting is interpreted. See
;; <http://github.com/guides/readme-formatting> for more information.

;; Emacs Lisp files customarily have a header in a fairly standardized
;; format. md-readme extracts this header, re-formats it to Markdown,
;; and writes it to the file "README.md" in the same directory. If you
;; put your code on github, you could have this run automatically, for
;; instance upon saving the file or from a git pre-commit hook, so you
;; always have an up-to-date README on github.

;; It recognizes headings, the GPL license disclaimer which is
;; replaced by a shorter notice linking to the GNU project's license
;; website, lists, and normal paragraphs. It escapes `backtick-quoted'
;; names so they will display correctly. Lists are somewhat tricky to
;; recognize automatically, and the program employs a very simple
;; heuristic currently.

;;; Dependencies:
;; None.

;;; Installation:
;; (require 'md-readme), then you can call mdr-generate manually. I
;; have not found a way to call it automatically that I really like,
;; but here is one that works for me:

;;     (require 'md-readme)
;;     (dir-locals-set-class-variables
;;      'generate-README-with-md-readme
;;      '((emacs-lisp-mode . ((mdr-generate-readme . t)))))
;;     (dolist (dir '("~/Projects/wpmail/" "~/Projects/md-readme/"))
;;       (dir-locals-set-directory-class
;;        dir 'generate-README-with-md-readme))
;;     (add-hook 'after-save-hook
;;               '(lambda () (if (boundp 'mdr-generate-readme) (mdr-generate))))

;;; Binaries
;; `bin/md-readme` is a shell script that will generate readme.md for the
;; passed file. See it for usage instructions.

;;; History:
;; 2009-11:    First release.

;;; Code:
(defun mdr-generate (&optional out-filename)
  "Generate README.md from the header of the current file."
  (interactive)
  (let ((header (mdr-extract-header)))
    (with-temp-file (or out-filename "README.md")
      (insert header)
      (mdr-convert-header))))

(defun mdr-generate-batch ()
  "Generate README.md from elisp files on the command line.
Takes two command line arguments: the elisp filename, and the target
Markdown filename (which defaults to 'README.md'."
  (let ((in-filename (expand-file-name (or (car command-line-args-left) "")))
        (out-filename (expand-file-name (or (cadr command-line-args-left) "README.md"))))
    (message "Generating %s from %s..." out-filename in-filename)
    (with-current-buffer (find-file in-filename)
      (mdr-generate out-filename))
    (setq command-line-args-left (cddr command-line-args-left))))

(defun mdr-convert-header ()
  "Convert the header to Markdown.
This function transforms the header in-place, so be sure to
extract the header first with mdr-extract-header and call it on
the copy."
  (goto-char (point-min))
  ;; Replace "separator" lines of just semicolons
  (replace-regexp "
;;;;* *
" "\n")
  (goto-char (point-min))
  ;; Collapse multiple blank comment lines to just one
  (replace-regexp "
\\(;; *\n\\)\\{2,\\}" "
\\1")
  (goto-char (point-min))
  (mdr-find-and-replace-disclaimer)
  (mdr-escape-quoted-names)
  (while (< (line-number-at-pos) (line-number-at-pos (point-max)))
    (when (looking-at ";;")
      (delete-char 2)
      (cond ((looking-at ";")  ; heading
	     (delete-char 1)
	     (if (looking-at " Code:?")
		 (delete-region (point) (line-end-position))
	       (insert "#")
	       (progn
		 (end-of-line)
		 (backward-char)
		 (when (looking-at ":")
		   (delete-char 1)))))
	    ((mdr-looking-at-list-p) (insert "*"))
            ((looking-at "\n") nil) ; Nothing after the semicolons
	    (t (delete-char 1)))) ; whitespace
    (forward-line 1)))

(defun mdr-extract-header ()
  "Extract the standard ELisp file header into a string."
  (buffer-substring (point-min) (mdr-end-of-header)))

(defun mdr-end-of-header ()
  "Find the end of the header and return its position."
  (save-excursion
    (goto-char (point-min))
    (while (or (looking-at "\n") (looking-at ";;"))
      (forward-line 1))
    (point)))

(defun mdr-looking-at-list-p ()
  "Determine if the line we're looking should become a list item.
Requires point to be at the beginning of the line."
  (looking-at " ?[-a-zA-Z0-9]+:"))  ; why does [:alnum:] not work?

(defun mdr-find-and-replace-disclaimer ()
  "Find the GPL license disclaimer, and replace it with a
one-line note linked to the GPL website."
  (save-excursion
    (goto-char (point-min))
    (when (search-forward "This program is free software" nil t)
      (let ((start-line (progn (beginning-of-line) (point)))
      	    (end-line (search-forward
      		       "If not, see <http://www.gnu.org/licenses/>."
      		       nil t)))
      	(delete-region start-line end-line)
      	(insert "Licensed under the [GPL version 3](http://www.gnu.org/licenses/) or later.")))))

(defun mdr-escape-quoted-names ()
  "Escape elisp-style backtick-quoted identifiers for use in a Markdown document.

Backticks have their own quoting semantics in Markdown, and they are at odds
with the ones used by Emacs Lisp.

Thus, this escaping is necessary."

  (save-excursion
    (goto-char (point-min))
    (replace-regexp "`\\(\\_<.*\\_>\\)'" "`` `\\1' ``")))

(provide 'md-readme)
;;; md-readme.el ends here
