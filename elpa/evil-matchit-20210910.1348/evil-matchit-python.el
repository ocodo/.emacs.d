;;; evil-matchit-python.el --- python plugin of evil-matchit

;; Copyright (C) 2014-2020 Chen Bin <chenbin DOT sh AT gmail DOT com>

;; Author: Chen Bin <chenbin DOT sh AT gmail DOT com>

;; This file is not part of GNU Emacs.

;;; License:

;; This file is part of evil-matchit
;;
;; evil-matchit is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; evil-matchit is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.



;;; Commentary:
;;

;;; Code:

(require 'evil-matchit-sdk)

(defun evilmi--python-calculate-indent (line)
  "Return number of indent of LINE."
  (let* (prefix)
    (cond
     ((string-match "^[ \t]*$" line)
      ;; empty line
      9999)

     ((string-match "^\\([ \t]+\\).*$" line)
      (setq prefix (match-string 1 line))
      ;; char code of tab is 9
      (if (= (elt prefix 0) 9)
          (length prefix)
        ;; Python developers always indent 4 spaces
        (/ (length prefix) 4)))
     (t
      ;; line begin at the first column
      0))))

(defun evilmi--python-move-to-first-open-tag (cur-indent)
 "Jump to the open tag based on CUR-INDENT.
For example, jump from the tag \"finally\" to \"try\".
Only python need this hack."
  (let* (out-of-loop
         keyword
         where-to-go
         regexp
         (cur-line (evilmi-sdk-curline)))

    ;; extract keyword from current line
    (if (string-match "^[ \t]*\\([a-z]+\\) *.*:[ \t]*\\(#.*\\)?$" cur-line)
        (setq keyword (match-string 1 cur-line)))

    (cond
     ((string= keyword "else")
      (setq regexp "^[ \t]*\\(if\\) *.*:[ \t]*\\(#.*\\)?$"))

     ((or (string= keyword "finally") (string= keyword "except"))
      (setq regexp "^[ \t]*\\(try\\) *.*:[ \t]*\\(#.*\\)?$")))

    (when regexp
      (save-excursion
        (while (not out-of-loop)
          (forward-line -1)
          (setq cur-line (evilmi-sdk-curline))

          (when (and (= cur-indent (evilmi--python-calculate-indent cur-line))
                     (string-match regexp cur-line))
            (setq where-to-go (line-beginning-position))
            (setq out-of-loop t))

          ;; if it's first line, we need get out of loop
          (if (= (point-min) (line-beginning-position))
              (setq out-of-loop t))))
      (when where-to-go
        (goto-char where-to-go)
        (skip-chars-forward " \t")))))

(defun evilmi--python-move-to-next-open-tag (keyword cur-indent)
  "Move to next open tag using KEYWORD and CUR-INDENT."
  (let* (out-of-loop
         where-to-go
         regexp
         cur-line)

    (cond
     ((string= keyword "try")
      (setq regexp "^[ \t]*\\(except\\) *.*:[ \t]*\\(#.*\\)?$"))

     ((string= keyword "except")
      (setq regexp "^[ \t]*\\(except\\|finally\\) *.*:[ \t]*\\(#.*\\)?$"))

     ((or (string= keyword "elif") (string= keyword "if"))
      (setq regexp "^[ \t]*\\(elif\\|else\\) *.*:[ \t]*\\(#.*\\)?$")))

    (save-excursion
      (while (not out-of-loop)
        (forward-line)
        (setq cur-line (evilmi-sdk-curline))

        (when (= cur-indent (evilmi--python-calculate-indent cur-line))
          (if (and regexp (string-match regexp cur-line))
              (setq where-to-go (line-beginning-position)))
          (setq out-of-loop t))
        ;; if it's last line, we need get out of loop
        (when (= (point-max) (line-end-position))
          (setq out-of-loop t))))

    (when where-to-go
      (goto-char where-to-go)
      (skip-chars-forward " \t"))))

;;;###autoload
(defun evilmi-python-get-tag ()
  "Return '(start-position tag-type keyword)."
  (let* (rlt
         (regexp "^[ \t]*\\([a-z]+\\) *.*:[ \t]*\\(#.*\\)?$")
         (cur-line (evilmi-sdk-curline))
         next-line)

    (if evilmi-debug (message "evilmi-python-get-tag called"))

    (cond
     ((string-match regexp cur-line)
      ;; we are at open tag now, and will jump forward
      (setq rlt (list (line-beginning-position)
                      0
                      (match-string 1 cur-line))))

     ((or (not (setq next-line (evilmi-next-non-empty-line)))
          (< (evilmi--python-calculate-indent next-line)
             (evilmi--python-calculate-indent cur-line)))
      ;; double check next line to make sure current line is close tag
      ;; if next line indention is less than current line or next line is empty line
      ;; we are at closed tag now, will jump backward
      (setq rlt (list (line-end-position) 1 "")))

     (t
      (setq rlt nil)))

    (if (and evilmi-debug rlt) (message "evilmi-python-get-tag called. rlt=%s" rlt))

    rlt))

;;;###autoload
(defun evilmi-python-jump (info num)
  "Use INFO returned by `evilmi-python-get-tag' and NUM to jump to matched tag."
  (ignore num)
  (let* ((p (nth 0 info))
         (tag-type (nth 1 info))
         (keyword (nth 2 info))
         (cur-line (evilmi-sdk-curline))
         (cur-indent (evilmi--python-calculate-indent cur-line))
         dendent
         rlt)

    (if evilmi-debug (message "evilmi-python-jump called. tag-type=%d p=%d" tag-type p))
    (cond
     ;; start from closed tag
     ((=  1 tag-type)
      ;; jump to back to open tag when current indentation is NOT zero
      (unless (= cur-indent 0)
        (goto-char p)
        (while (not dendent)
          (forward-line -1)
          ;; first line
          (setq cur-line (evilmi-sdk-curline))

          (if evilmi-debug (message "cur-line=%s" cur-line))

          ;; skip empty lines
          (when (and (not (string-match "^[ \t]*$" cur-line))
                     (< (evilmi--python-calculate-indent cur-line) cur-indent))
            (setq dendent t)
            (skip-chars-forward " \t")
            (evilmi--python-move-to-first-open-tag (1- cur-indent))
            (setq rlt (point))))))

     ;; start from open tag
     ((=  0 tag-type)
      ;; jump to closed tag
      (while (not dendent)
        (forward-line)
        (setq cur-line (evilmi-sdk-curline))

        ;; just skip empty line
        (if (not (string-match "^[ \t]*$" cur-line))
            (if (<= (evilmi--python-calculate-indent cur-line) cur-indent)
                (setq dendent t)
              ;; record the latest indented line info
              (setq rlt (line-end-position))))
        ;; last line
        (if (= (point-max) (line-end-position)) (setq dendent t)))

      (if rlt (goto-char rlt))

      (evilmi--python-move-to-next-open-tag keyword cur-indent)))
    rlt))

(provide 'evil-matchit-python)
;;; evil-matchit-python.el ends here
