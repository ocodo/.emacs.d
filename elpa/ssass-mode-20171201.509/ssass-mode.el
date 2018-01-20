;;; ssass-mode.el --- Edit Sass without a Turing Machine

;; Copyright 2017 Adam Niederer

;; Author: Adam Niederer <adam.niederer@gmail.com>
;; URL: http://github.com/AdamNiederer/ssass-mode
;; Package-Version: 20171201.509
;; Version: 0.1
;; Keywords: languages sass
;; Package-Requires: ((emacs "24.3"))

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; This mode is a clone of sass-mode which works in mmm-mode and doesn't
;; indent things as eagerly.  Syntax highlighting is provided with
;; `font-lock-mode'.
;;
;; Exported names start with "ssass-"; private names start with
;; "ssass--".

;;; Code:

(defgroup ssass nil
  "Major mode for Sass files"
  :prefix "ssass-"
  :group 'languages
  :link '(url-link :tag "Github" "https://github.com/AdamNiederer/ssass-mode")
  :link '(emacs-commentary-link :tag "Commentary" "ssass-mode"))

(defconst ssass-id-regex
  "#[a-z][A-z0-9\-]+")

(defconst ssass-class-regex
  "\\.[a-z][A-z0-9\-]+")

(defconst ssass-pseudoselector-regex
  "::?[A-z0-9\-]+")

(defconst ssass-key-regex
  "^\s+[a-z\-]+:")

(defconst ssass-directive-noindent-regex
  "@\\(include\\|extend\\|import\\|warn\\|debug\\|error\\)"
  "Matches all directives which do not require indentation.")

(defconst ssass-variable-regex
  "\$[A-z\-]+")

(defconst ssass-builtin-regex
  "@[A-z]+")

(defconst ssass-comment-regex
  "^\s+/[/*].*") ; TODO: Make better or use syntax table

(defconst ssass-function-regex
  "\\([A-z\-]+?\\)\\((.*)\\)")

(defconst ssass-keywords
  '("and" "or" "not" "in" "@if" "@else" "@each" "@for" "@return"))

(defconst ssass-constants
  '("true" "false" "null"))

(defconst ssass-bang-regex
  "![a-z][A-z0-9]+")

(defcustom ssass-tab-width 2
  "Tab width for ‘ssass-mode’."
  :group 'ssass
  :type 'integer)

(defcustom ssass-compiler "sassc"
  "Sass compiler for `ssass-eval-region' and `ssass-eval-buffer'."
  :group 'ssass
  :type 'string)

(defcustom ssass-opt "--sass"
  "Options for `ssass-compiler'.

Use --sass for sassc, and --indented-syntax for node-sass."
  :group 'ssass
  :type 'string)

(defcustom ssass-color-keys nil
  "(TODO) Whether to color proprty names."
  :group 'ssass
  :type 'boolean)

(defconst ssass-font-lock-keywords
  `((,(regexp-opt ssass-keywords 'words) . font-lock-keyword-face)
    (,(regexp-opt ssass-constants 'words) . font-lock-constant-face)
    (,ssass-id-regex . (0 font-lock-keyword-face))
    (,ssass-class-regex . (0 font-lock-type-face))
    (,ssass-builtin-regex . (0 font-lock-builtin-face))
    (,ssass-key-regex . (0 font-lock-variable-name-face))
    (,ssass-function-regex . (1 font-lock-function-name-face))
    (,ssass-pseudoselector-regex . (0 font-lock-function-name-face))
    (,ssass-variable-regex . (0 font-lock-variable-name-face))
    (,ssass-bang-regex . (0 font-lock-warning-face)))
  "List of Font Lock keywords.")

(defvar ssass-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "<backtab>") 'ssass-dedent)
    (define-key map (kbd "C-c C-c") 'ssass-eval-buffer)
    (define-key map (kbd "C-c C-r") 'ssass-eval-region)
    map)
  "Keymap for ‘ssass-mode’.")

(defun ssass--selector-p (line)
  "Return whether LINE is a selector."
  (not (or (string-empty-p line)
           (string-match-p ssass-key-regex line)
           (string-match-p ssass-variable-regex line)
           (string-match-p ssass-directive-noindent-regex line)
           (string-match-p ssass-comment-regex line))))

(defun ssass--goto-last-selector-line ()
  "Move point to the line of the last selector, or the beginning of the buffer."
  (forward-line -1)
  (while (not (or (equal (point-min) (point-at-bol))
                  (ssass--selector-p (buffer-substring (point-at-bol) (point-at-eol)))))
    (forward-line -1)))

(defun ssass--last-selector-line-indent-level ()
  "Return the number of spaces indenting the line of the last selector."
  (save-excursion
    (ssass--goto-last-selector-line)
    (ssass--indent-level)))

(defun ssass--indent-level ()
  "Return the number of spaces indenting the current line."
  (- (save-excursion
       (back-to-indentation)
       (current-column))
     (save-excursion
       (beginning-of-line)
       (current-column))))

(defun ssass--whitespace-before-p ()
  "Return whether the previous line consists solely of whitespace."
  (save-excursion
    (forward-line -1)
    (string-match-p "^[[:space:]]*$" (buffer-substring (point-at-bol) (point-at-eol)))))

(defun ssass--comma-before-p ()
  "Return whether the previous line has a comma at its end."
  (save-excursion
    (forward-line -1)
    (string-match-p ".*," (buffer-substring (point-at-bol) (point-at-eol)))))

(defun ssass--no-selector-line-p ()
  "Return whether there is no proper selector or keyword above this line."
  (save-excursion
    (ssass--goto-last-selector-line)
    (not (ssass--selector-p (buffer-substring (point-at-bol) (point-at-eol))))))

(defun ssass-indent ()
  "Indent the current line."
  (interactive)
  (indent-line-to
   (cond
    ((ssass--whitespace-before-p) 0)
    ((ssass--no-selector-line-p) 0)
    ((ssass--comma-before-p) (ssass--last-selector-line-indent-level))
    (t (+ ssass-tab-width (ssass--last-selector-line-indent-level))))))

(defun ssass-dedent ()
  "Remove one level of indentation from the current line."
  (interactive)
  (indent-line-to (max 0 (- (ssass--indent-level) ssass-tab-width))))

(defun ssass-eval-file (&optional filename)
  "Run the given file through sass, and display the output in another window.

If FILENAME is nil, it will open the current buffer's file"
  (interactive)
  (when (buffer-live-p (get-buffer "*sass*"))
    (kill-buffer "*sass*"))
  (start-process "sass" "*sass*" ssass-compiler ssass-opt (or filename (buffer-file-name)))
  (switch-to-buffer-other-window "*sass*")
  (special-mode))

(defun ssass-eval-region (beg end)
  "Run the region from BEG to END through sass, and display the output in another window."
  (interactive "r")
  (let ((tmp-file (make-temp-file "sass-eval" nil ".sass")))
    (write-region beg end tmp-file nil nil nil nil)
    (ssass-eval-file tmp-file)
    (delete-file tmp-file))  )

(defun ssass-eval-buffer ()
  "Run the current buffer through sass, and display the output in another window."
  (interactive)
  (ssass-eval-region (point-min) (point-max)))

;;;###autoload
(define-derived-mode ssass-mode prog-mode "Ssass"
  "Major mode for Sass"
  (setq-local electric-indent-mode nil)
  (setq tab-width ssass-tab-width)
  (setq indent-line-function 'ssass-indent)
  (font-lock-add-keywords nil ssass-font-lock-keywords)
  (modify-syntax-entry ?/ ". 124" ssass-mode-syntax-table)
  (modify-syntax-entry ?* ". 23b" ssass-mode-syntax-table)
  (modify-syntax-entry ?\n ">" ssass-mode-syntax-table))

(provide 'ssass-mode)
;;; ssass-mode.el ends here
