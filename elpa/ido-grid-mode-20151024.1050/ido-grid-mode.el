;;; ido-grid-mode.el --- Display ido-prospects in the minibuffer in a grid. -*- coding: utf-8; lexical-binding: t -*-

;; Copyright (C) 2015  Tom Hinton

;; Author: Tom Hinton
;; Maintainer: Tom Hinton <t@larkery.com>
;; Version: 1.0.1
;; Package-Version: 20151024.1050
;; Keywords: convenience
;; URL: https://github.com/larkery/ido-grid-mode.el
;; Package-Requires: ((emacs "24.4"))

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

;; Makes ido-mode display prospects in a grid. The mechanism is based
;; on ido-vertical-mode, but it is sufficiently different that I
;; reimplemented it. The purpose is to look a bit like zsh style
;; completion lists.  Most of the behaviour can be customized, in the
;; ido-grid-mode group.  relevant variables are
;; `ido-grid-mode-min-rows', `ido-grid-mode-max-rows',
;; `ido-grid-mode-keys', `ido-grid-mode-start-collapsed'.  If you want
;; ido-grid-mode to sometimes be more horizontal or more vertical, you
;; can let `ido-grid-mode-max-rows' or `ido-grid-mode-max-columns'
;; around the call you are interested in (or in advice around an
;; existing command).

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'ido)

;;; The following four variables and the first three comments are lifted
;;; directly from `ido.el'; they are defined here to avoid compile-log
;;; warnings. See `ido.el' for more information.

;; Non-nil if we should add [confirm] to prompt
(defvar ido-show-confirm-message)

;; Remember if current directory is non-readable (so we cannot do completion).
(defvar ido-directory-nonreadable)

;; Remember if current directory is 'huge' (so we don't want to do completion).
(defvar ido-directory-too-big)

(defvar ido-report-no-match t
  "Report [No Match] when no completions matches ido-text.")

(defvar ido-matches nil
  "List of files currently matching `ido-text'.")

(defvar ido-incomplete-regexp nil
  "Non-nil if an incomplete regexp is entered.")

;; this is defined dynamically by ido
(eval-when-compile
  (defvar ido-cur-list))

;; custom settings

(defgroup ido-grid-mode nil
  "Displays ido prospects in a grid in the minibuffer."
  :group 'ido)

(defcustom ido-grid-mode-max-columns nil
  "The maximum number of columns - nil means no maximum."
  :type '(choice 'integer (const :tag "Unlimited" nil))
  :group 'ido-grid-mode)

(defcustom ido-grid-mode-max-rows 5
  "The maximum number of rows."
  :type 'integer
  :group 'ido-grid-mode)

(defcustom ido-grid-mode-min-rows 5
  "The minimum number of rows."
  :type 'integer
  :group 'ido-grid-mode)

(defcustom ido-grid-mode-order t
  "The order to put things in the grid."
  :type '(choice (const :tag "Row-wise (row 1, then row 2, ...)" nil)
                 (const :tag "Column-wise (column 1, then column 2, ...)" t))
  :group 'ido-grid-mode)

(defcustom ido-grid-mode-jank-rows 1000
  "Only this many rows will be considered when packing the grid.
If this is a low number, the column widths will change more when scrolling."
  :type 'integer
  :group 'ido-grid-mode)

(defcustom ido-grid-mode-padding "    "
  "The padding text to put between columns - this can contain characters like | if you like."
  :type 'string
  :group 'ido-grid-mode)

(defcustom ido-grid-mode-first-line '(" [" ido-grid-mode-count "]")
  "How to generate the top line of input.
This can be a list of symbols; function symbols will be
evaluated.  The function `ido-grid-mode-count' displays a count
of visible and matching items.  `ido-grid-mode-long-count'
displays more detail about this."
  :type '(repeat (choice function symbol string))
  :group 'ido-grid-mode)

(defcustom ido-grid-mode-exact-match-prefix ">> "
  "A string to put before an exact match."
  :type 'string
  :group 'ido-grid-mode)

(defcustom ido-grid-mode-prefix "-> "
  "A string to put at the start of the first row when there isn't an exact match."
  :type 'string
  :group 'ido-grid-mode)

(defcustom ido-grid-mode-prefix-scrolls nil
  "Whether the prefix arrow should go on the row where the current item is."
  :type 'boolean
  :group 'ido-grid-mode)

(defface ido-grid-mode-match
  '((t (:underline t)))
  "The face used to mark up matching groups when showing a regular expression."
  :group 'ido-grid-mode)

(defface ido-grid-mode-common-match
  '((t (:inherit shadow)))
  "The face used to display the common match prefix."
  :group 'ido-grid-mode)

(defcustom ido-grid-mode-always-show-min-rows t
  "Whether to expand the minibuffer to be `ido-grid-mode-min-rows' under all circumstances (like when there is a single match, or an error in the input)."
  :group 'ido-grid-mode
  :type 'boolean)

(defcustom ido-grid-mode-keys '(tab backtab up down left right C-n C-p)
  "Which keys to reconfigure in the minibuffer.

Tab and backtab will move to the next/prev thing, arrow keys will
move around in the grid, and C-n, C-p will scroll the grid in
pages."
  :group 'ido-grid-mode
  :type '(set (const tab) (const backtab) (const up) (const down) (const left) (const right) (const C-n) (const C-p)))

(defcustom ido-grid-mode-advise-perm '(ido-exit-minibuffer)
  "Functions which will want to see the right thing at the head of the ido list."
  :type 'hook
  :options '(ido-exit-minibuffer)
  :group 'ido-grid-mode)

(defcustom ido-grid-mode-advise-temp '(ido-kill-buffer-at-head ido-delete-file-at-head)
  "Functions which will refer to `ido-matches', but will return to ido later.
If you've added stuff to ido which operates on the current match, pop it in this list."
  :type 'hook
  :options '(ido-kill-buffer-at-head ido-delete-file-at-head)
  :group 'ido-grid-mode)

(defcustom ido-grid-mode-start-collapsed nil
  "If t, ido-grid-mode shows one line when it starts, and displays the grid when you press tab.

Note that this depends on `ido-grid-mode-keys' having tab
enabled; if it is not, bind something to `ido-grid-mode-tab' to un-collapse."
  :type 'boolean
  :group 'ido-grid-mode)

(defcustom ido-grid-mode-scroll-down #'ido-grid-mode-next-page
  "The function which will be called when the cursor is moved beyond the end of the grid.
Consider `ido-grid-mode-next-page' or `ido-grid-mode-next-row'.
Next row is only really sensible when `ido-grid-mode-order' is row-wise, and the column count is small and fixed.."
  :type 'function
  :group 'ido-grid-mode)

(defcustom ido-grid-mode-scroll-up #'ido-grid-mode-previous-page
  "The function which will be called when the cursor is moved before the end of the grid.
Consider `ido-grid-mode-previous-page' or `ido-grid-mode-previous-row'.
Previous row is only really sensible when `ido-grid-mode-order' is row-wise, and the column count is small and fixed."
  :type 'function
  :group 'ido-grid-mode)

(defcustom ido-grid-mode-scroll-wrap t
  "Whether to scroll the grid when hitting an edge, or to wrap
  around. Scrolling always happens at the top left or bottom right."
  :type 'boolean
  :group 'ido-grid-mode)

;; vars
(defvar ido-grid-mode-rows 0)
(defvar ido-grid-mode-columns 0)
(defvar ido-grid-mode-count 0)
(defvar ido-grid-mode-offset 0)
(defvar ido-grid-mode-common-match nil)

(defvar ido-grid-mode-collapsed nil)

(defun ido-grid-mode-row-major ()
  "Is the grid row major?"
  (not ido-grid-mode-order))
(defun ido-grid-mode-column-major ()
  "Is the grid column major?"
  ido-grid-mode-order)

(defvar ido-grid-mode-lengths-cache
  (make-hash-table :test 'equal :weakness t))

(defun ido-grid-mode-mapcar (fn stuff)
  "Map FN over some STUFF, storing the result in a weak cache."
  (let* ((key (cons fn stuff))
         (existing (gethash key ido-grid-mode-lengths-cache)))
    (or existing (puthash key (mapcar fn stuff)
                          ido-grid-mode-lengths-cache))))

(defmacro ido-grid-mode-debug (_s)
  ;; `(with-current-buffer
  ;;      (get-buffer-create "ido-grid-debug")
  ;;    (end-of-buffer)
  ;;    (insert ,_s)
  ;;    (insert "\n"))
  )

;; Compute the number of columns to use. This consumes about half the runtime,
;; and it could be a pure function

(defun ido-grid-mode-count-columns-pure
    (lengths
     max-width

     ;; these are all defvars which are passed
     ;; so this can be memoized
     -padding
     -jank-rows
     -max-columns
     -min-rows
     -max-rows

     -row-major)

  (let* ((padding (length -padding))
         (lower 1)
         (item-count (length lengths))
         (jank (> item-count -jank-rows))
         (upper (min
                 (or -max-columns max-width)
                 (if -row-major
                     (1+ (/ max-width (apply #'min lengths)))
                   (+ 2 (/ item-count -min-rows)))))
         lower-solution)

    (while (< lower upper)
      (let* ((middle (+ lower (/ (- upper lower) 2)))
             (rows (max -min-rows
                        (min -max-rows
                             (/ (+ (- middle 1) item-count) middle))))

             (spare-width (- max-width (* padding (- middle 1))))
             (total-width 0)
             (column 0)
             (row 0)
             (widths (make-vector middle 0)))

        ;; try and pack the items
        (let ((overflow
               (catch 'stop
                 (dolist (length lengths)
                   ;; if we have reached the jank point, stop
                   (when (and -row-major
                              jank
                              (>= row -max-rows))

                     (throw 'stop nil))

                   (when (and (not -row-major)
                              (>= column middle))
                     (throw 'stop nil))

                   ;; see if this grows the current column

                   (let ((w (aref widths column)))
                     (when (> length w)
                       (cl-incf total-width (- length w))
                       (aset widths column length)))

                   ;; bump counters
                   (if -row-major
                       (progn (setq column (% (1+ column) middle))
                              (when (zerop column) (cl-incf row)))
                     (progn (setq row (% (1+ row) rows))
                            (when (zerop row) (cl-incf column))))

                   ;; die if overflow
                   (when (> total-width spare-width)
                     (throw 'stop t))))))

          ;; move bound in search
          (if overflow
              (setq upper middle)
            (progn (setq lower middle
                         lower-solution widths)
                   (if (= (1+ lower) upper)
                       (setq upper lower))))
          )))
    (cl-remove-if #'zerop lower-solution)
    ))

(defvar ido-grid-mode-count-columns-cache
  (make-hash-table :test 'equal :weakness 'key))

(defun ido-grid-mode-count-columns (lengths max-width)
  "Packing items of LENGTHS into MAX-WIDTH, what columns are needed?.
The items will be placed into columns row-wise, so the first row
will contain the first k items, and so on.  The result is a
vector of column widths, or nil if even 1 column is too many.
Refers to `ido-grid-mode-order' to decide whether to try and fill
rows or columns."
  (let* ((args (list lengths
                     max-width

                     ido-grid-mode-padding
                     ido-grid-mode-jank-rows
                     ido-grid-mode-max-columns
                     ido-grid-mode-min-rows
                     ido-grid-mode-max-rows
                     (ido-grid-mode-row-major)))
         (result (gethash args ido-grid-mode-count-columns-cache)))
    (or result
        (puthash args (apply #'ido-grid-mode-count-columns-pure args)
                 ido-grid-mode-count-columns-cache))))

;; functions to layout text in a grid of known dimensions.

(defun ido-grid-mode-pad (string current-length desired-length)
  "Given a STRING of CURRENT-LENGTH, pad it to the DESIRED-LENGTH with spaces, if it is shorter."
  (let ((delta (- desired-length current-length)))
    (cond ((zerop delta) string)
          ((> delta 0) (concat string (make-list delta 32)))
          (t string))))

(defun ido-grid-mode-copy-name (item)
  "Copy the `ido-name' of ITEM into a new string."
  (substring (ido-name item) 0))

(defun ido-grid-mode-string-width (s)
  "The displayed width of S in the minibuffer, excluding invisible text."
  (let* ((base-length (length s))
         (onset (text-property-any 0 base-length 'invisible t s)))
    (if onset
        (let ((base-width (string-width s))
              (offset 0))

          (while onset
            (setq offset (text-property-any onset base-length 'invisible nil s))
            (cl-decf base-width (string-width (substring s onset offset)))
            (setq onset (text-property-any offset base-length 'invisible t s)))

          base-width)
      (string-width s))))

(cl-defun ido-grid-mode-gen-grid (items
                                  &key
                                  name
                                  decorate
                                  max-width)
  "Generate string which lays out the given ITEMS to fit in MAX-WIDTH. Also refers to `ido-grid-min-rows' and `ido-grid-max-rows', etc.
NAME will be used to turn ITEMS into strings, and the DECORATE to fontify them based on their location and name.
Modifies `ido-grid-mode-rows', `ido-grid-mode-columns', `ido-grid-mode-count' and sometimes `ido-grid-mode-offset' as a side-effect, sorry."
  (let* ((row-padding (make-list (length ido-grid-mode-prefix) 32))
         (names (ido-grid-mode-mapcar name items))
         (lengths (ido-grid-mode-mapcar #'ido-grid-mode-string-width names))
         (padded-width (- max-width (length row-padding)))
         (col-widths (or (ido-grid-mode-count-columns lengths padded-width)
                         (make-vector 1 padded-width)))
         (col-count (length col-widths))
         (row-count (max ido-grid-mode-min-rows
                         (min ido-grid-mode-max-rows
                              (/ (+ (length names) (- col-count 1)) col-count))))
         (grid-size (* row-count col-count))
         (col 0)
         (row 0)
         (index 0)
         indicator-row
         all-rows)

    (add-face-text-property 0 (length ido-grid-mode-prefix)
                            'minibuffer-prompt
                            nil ido-grid-mode-prefix)

    (setq ido-grid-mode-rows    row-count
          ido-grid-mode-columns col-count)

    (setq ido-grid-mode-count (min (* ido-grid-mode-rows ido-grid-mode-columns)
                                   (length items)))

    (setq ido-grid-mode-offset (max 0 (min ido-grid-mode-offset (- ido-grid-mode-count 1))))
    (setq indicator-row (if ido-grid-mode-prefix-scrolls
                            (if (ido-grid-mode-row-major)
                                (/ ido-grid-mode-offset col-count)
                              (% ido-grid-mode-offset row-count))
                          0))

    (if (ido-grid-mode-row-major)
        ;; this is the row-major code, which is easy
        (while (and names (< row row-count))
          (push
           (if (zerop col)
               (if (= row indicator-row) ido-grid-mode-prefix row-padding)
             ido-grid-mode-padding)
           all-rows)

          (push (ido-grid-mode-pad (funcall decorate (pop names) (pop items) row col index)
                                   (pop lengths)
                                   (aref col-widths col)) all-rows)

          (cl-incf index)
          (cl-incf col)

          (when (= col col-count)
            (setq col 0)
            (cl-incf row)
            (if (< row row-count) (push "\n" all-rows))))

      ;; column major:
      (let ((row-lists (make-vector row-count nil)))
        (while (and names (< index grid-size))
          (setq row (% index row-count)
                col (/ index row-count))

          (push
           (if (zerop col)
               (if (= row indicator-row)
                   ido-grid-mode-prefix row-padding)
             ido-grid-mode-padding)
           (elt row-lists (- row-count (1+ row))))

          (push (ido-grid-mode-pad (funcall decorate (pop names) (pop items)
                                            row col index)
                                   (pop lengths)
                                   (aref col-widths col))
                (elt row-lists (- row-count (1+ row))))

          (cl-incf index))

        (dotimes (i (- (length row-lists) 1))
          (push "\n" (aref row-lists (- (length row-lists) 1 i))))

        ;; now we have row-lists
        ;; each one is a row, and we want to put them
        ;; all into all-rows, with "\n" as well
        ;; each row is backwards
        (setq all-rows (apply #'nconc (append row-lists nil)))
        ))

    (list (apply #'concat (nreverse all-rows))
          row-count
          col-count)))

;; functions to produce the whole text
;; not going to respect ido-use-faces

(defun ido-grid-mode-gen-first-line ()
  "Generate the first line suffix text using `ido-grid-mode-first-line' hook."
  (concat ido-grid-mode-common-match
          (mapconcat (lambda (x)
                       (cond
                        ((functionp x) (or (funcall x) ""))
                        ((symbolp x) (format "%s" (or (eval x) "")))
                        (t (format "%s" x))))
                     ido-grid-mode-first-line
                     "")))

(defun ido-grid-mode-no-matches ()
  "If `ido-matches' is emtpy, produce a helpful string about it."
  (unless ido-matches
    (cond (ido-show-confirm-message  " [Confirm]")
          (ido-directory-nonreadable " [Not readable]")
          (ido-directory-too-big     " [Too big]")
          (ido-report-no-match       " [No match]")
          (t ""))
    ))

(defun ido-grid-mode-incomplete-regexp ()
  "If `ido-incomplete-regexp', return the first match coloured using the relevant face."
  (when ido-incomplete-regexp
    (concat " "
            (let ((name (ido-grid-mode-copy-name (car ido-matches))))
              (add-face-text-property 0 (length name) 'ido-incomplete-regexp nil name)
              name))))

(defun ido-grid-mode-exact-match ()
  "If there is a single match, return just that."
  (when (not (cdr ido-matches))
    (add-face-text-property 0 (length ido-grid-mode-exact-match-prefix)
                            'minibuffer-prompt
                            nil ido-grid-mode-exact-match-prefix)
    (concat
     (ido-grid-mode-gen-first-line) "\n"
     ido-grid-mode-exact-match-prefix
     (let ((name (ido-grid-mode-copy-name (car ido-matches))))
       (add-face-text-property
        0 (length name)
        'ido-only-match nil name)
       name))))

(defun ido-grid-mode-highlight-matches (re s)
  "Highlight matching groups for RE in S.
Given a regex RE and string S, add `ido-vertical-match-face' to
all substrings of S which match groups in RE.  If there are no
groups, add the face to all of S."
  (when (string-match re s)
    (ignore-errors
      ;; try and match each group in case it's a regex with groups
      (let ((group 1))
        (while (match-beginning group)
          (add-face-text-property (match-beginning group)
                                  (match-end group)
                                  'ido-grid-mode-match
                                  nil s)
          (cl-incf group))
        ;; it's not a regex with groups, so just mark the whole match region.
        (when (= 1 group)
          (add-face-text-property (match-beginning 0)
                                  (match-end 0)
                                  'ido-grid-mode-match
                                  nil s)
          )))))

(defun ido-grid-mode-merged-indicator (item)
  "Generate the merged indicator string for ITEM."
  (if (and (consp item)
           (sequencep (cdr item))
           (> (length (cdr item)) 1))
      (let ((mi (substring ido-merged-indicator 0)))
        (add-face-text-property 0 (length mi) 'ido-indicator nil mi)
        mi)
    ""))

(defun ido-grid-mode-grid (name)
  "Draw the grid for input NAME."
  (let* ((decoration-regexp (if ido-enable-regexp name (regexp-quote name)))
         (max-width (- (window-body-width (minibuffer-window)) 1))
         (decorator (lambda (name item _row _column offset)
                      (concat
                       (let ((name (substring name 0))
                             (l (length name)))
                         ;; copy the name so we can add faces to it
                         (when (and (/= offset ido-grid-mode-offset) ; directories get ido-subdir
                                    (ido-final-slash name))
                           (add-face-text-property 0 l 'ido-subdir nil name))
                         ;; selected item gets special highlight
                         (when (= offset ido-grid-mode-offset)
                           (add-face-text-property 0 l 'ido-first-match nil name))
                         (ido-grid-mode-highlight-matches decoration-regexp name)
                         name)
                       (ido-grid-mode-merged-indicator item))
                      ))
         (generated-grid (ido-grid-mode-gen-grid
                          ido-matches
                          :name #'ido-name
                          :decorate decorator
                          :max-width max-width))
         (first-line (ido-grid-mode-gen-first-line)))

    (concat first-line "\n" (nth 0 generated-grid))))

(defun ido-grid-mode-pad-missing-rows (s)
  "Pad out S to at least `ido-grid-mode-min-rows'."
  (if ido-grid-mode-always-show-min-rows
      (let ((rows 0))
        (dotimes (i (length s))
          (when (= (aref s i) 10)
            (cl-incf rows)))
        (if (< rows ido-grid-mode-min-rows)
            (apply #'concat (cons s (make-list (- ido-grid-mode-min-rows rows) "\n")))
          s))
    s))

(defun ido-grid-mode-completions (name)
  "Generate the prospect grid for input NAME."
  (setq ido-grid-mode-rows 1
        ido-grid-mode-columns 1
        ido-grid-mode-count 1)

  (let ((ido-grid-mode-common-match
         (and (stringp ido-common-match-string)
              (> (length ido-common-match-string) (length name))
              (substring ido-common-match-string (length name)))))
    (when ido-grid-mode-common-match
      (add-face-text-property 0 (length ido-grid-mode-common-match) 'ido-grid-mode-common-match nil ido-grid-mode-common-match))

    (let ((ido-grid-mode-max-rows    (if ido-grid-mode-collapsed 1 ido-grid-mode-max-rows))
          (ido-grid-mode-min-rows    (if ido-grid-mode-collapsed 1 ido-grid-mode-min-rows))
          (ido-grid-mode-order   (if ido-grid-mode-collapsed 'rows ido-grid-mode-order))
          (ido-grid-mode-jank-rows (if ido-grid-mode-collapsed 0 ido-grid-mode-jank-rows))
          (ido-grid-mode-always-show-min-rows (if ido-grid-mode-collapsed nil ido-grid-mode-always-show-min-rows)))

      (ido-grid-mode-pad-missing-rows
       (or (ido-grid-mode-no-matches)
           (ido-grid-mode-incomplete-regexp)
           (ido-grid-mode-exact-match)
           (ido-grid-mode-grid name)
           )))))

(defun ido-grid-mode-long-count ()
  "For use in `ido-grid-mode-first-line.
Produces a string like '10/20, 8 not shown'
to say that there are 20 candidates, of
which 10 match, and 8 are off-screen."
  (let ((count (length ido-matches))
        (cand (length ido-cur-list))
        (vis ido-grid-mode-count))
    (if (< vis count)
        (format "%d/%d, %d not shown" count cand (- count vis))
      (format "%d/%d" count cand))))

(defun ido-grid-mode-count ()
  "For use in `ido-grid-mode-first-line'.
Counts matches, and tells you how many you can see in the grid."
  (let ((count (length ido-matches)))
    (if (> count ido-grid-mode-count)
        (format "%d/%d" ido-grid-mode-count count)
      (number-to-string count))))

;; movement in grid keys

;; if row major

;; 1 2 3 4
;; 5 6 7 8

(defun ido-grid-mode-move (dr dc)
  "Move `ido-grid-mode-offset' by DR rows and DC cols."
  (let* ((nrows ido-grid-mode-rows)
         (ncols ido-grid-mode-columns)

         (row   (if (ido-grid-mode-row-major)
                    (/ ido-grid-mode-offset ido-grid-mode-columns)
                  (% ido-grid-mode-offset ido-grid-mode-rows)))
         (col   (if (ido-grid-mode-row-major)
                    (% ido-grid-mode-offset ido-grid-mode-columns)
                  (/ ido-grid-mode-offset ido-grid-mode-rows))))

    (cl-incf row dr)
    (cl-incf col dc)

    (unless (or (and (= col 0)
                     (= row -1))
                (and (= row nrows)
                     (= (1+ col) ncols))
                (and (not ido-grid-mode-scroll-wrap)
                     (if (ido-grid-mode-row-major)
                         (not (< -1 row nrows))
                       (not (< -1 col ncols)))))

      (while (< row 0)
        (cl-decf col)
        (cl-incf row nrows))

      (while (>= row nrows)
        (cl-incf col)
        (cl-decf row nrows))

      (while (< col 0)
        (cl-decf row)
        (cl-incf col ncols))

      (while (>= col ncols)
        (cl-incf row)
        (cl-decf col ncols)))

    (cond ((or (< row 0) (< col 0))  ; this is the case where we went upwards or left from the top
           (funcall ido-grid-mode-scroll-up))

          ;; this is the case where we have scrolled down
          ((or (= row nrows) (= col ncols))
           (funcall ido-grid-mode-scroll-down))

          (t
           (setq ido-grid-mode-offset
                 (if (ido-grid-mode-row-major)
                     (+ col (* row ncols))
                   (+ row (* col nrows))))))
    ))

(defun ido-grid-mode-left ()
  "Move left in the grid."
  (interactive)
  (ido-grid-mode-move 0 -1))

(defun ido-grid-mode-right ()
  "Move right in the grid."
  (interactive)
  (ido-grid-mode-move 0 1))

(defun ido-grid-mode-up ()
  "Move up in the grid."
  (interactive)
  (ido-grid-mode-move -1 0))

(defun ido-grid-mode-down ()
  "Move down in the grid."
  (interactive)
  (ido-grid-mode-move 1 0))

(defun ido-grid-mode-previous ()
  "Move up or left in the grid."
  (interactive)
  (if (ido-grid-mode-row-major)
      (call-interactively #'ido-grid-mode-left)
    (call-interactively #'ido-grid-mode-up)))

(defun ido-grid-mode-next ()
  "Move down or right in the grid."
  (interactive)
  (if (ido-grid-mode-row-major)
      (call-interactively #'ido-grid-mode-right)
    (call-interactively #'ido-grid-mode-down)))

(defun ido-grid-mode-tab ()
  "Move to the next thing in the grid, or show the grid."
  (interactive)
  (if (and ido-grid-mode-collapsed (< ido-grid-mode-count (length ido-matches)))
      (setq ido-grid-mode-collapsed nil)
    (call-interactively #'ido-grid-mode-next)))

(defun ido-grid-mode-previous-page ()
  "Page up in the grid."
  (interactive)
  (ido-grid-mode-previous-N ido-grid-mode-count)
  (setq ido-grid-mode-offset (- ido-grid-mode-count 1)))

(defun ido-grid-mode-next-page ()
  "Page down in the grid."
  (interactive)
  (ido-grid-mode-next-N ido-grid-mode-count)
  (setq ido-grid-mode-offset 0))

(defun ido-grid-mode-previous-row ()
  "Scroll one up stride in the grid, kind of.
It may not be possible to do this unless there is only 1 column."
  (interactive)
  (ido-grid-mode-previous-N (if (ido-grid-mode-row-major)
                                  ido-grid-mode-columns
                                ido-grid-mode-rows)))

(defun ido-grid-mode-next-row ()
  "Scroll down one stride in the grid, kind of.
It may not be possible to do this unless there is only 1 column."
  (interactive)
  (ido-grid-mode-next-N (if (ido-grid-mode-row-major)
                            ido-grid-mode-columns
                          ido-grid-mode-rows)))

(defun ido-grid-mode-rotate-n (n matches items)
  "Because ido's prospects are produced by filtering MATCHES to ITEMS,
normal -rotate can't be used; we have to modify the ITEMS so that MATCHES
appears rotated.

This appears to break smex quite badly."

  ;; example (matches are uppercase)
  ;; (A B c d e F G H i j) + 2
  ;; A B F_G H + 2 =>
  ;; G H A B F
  ;; we could collect up items and move them

  ;; the last two items in matches need to be in front of the rest?

  ;; find cell with G in items
  ;; break the cell before
  ;; append the first cell
  ;; G is the new head

  ;; this is what ido chop does

  ;; the reverse direction
  ;; A B_F G H X Y Z - 2 =>
  ;; F G H X Y Z A B
  ;; is this essentially the same operation?

  (let* ((items (copy-sequence items))
         (match-count (length matches))
         (n (if (< n 0) (+ match-count n) n))
         (new-head (nth n matches))
         (walker items)
         new-tail)

    (while walker
      (if (eq new-head (cadr walker))
          (setq new-tail walker
                walker nil)
        (setq walker (cdr walker))))

    ;; splitting point's cdr is the cell whose car is new-head
    (setq new-head (cdr new-tail)) ;; find new head
    (setcdr new-tail nil) ;; break the tie
    (nconc new-head items)))

(defun ido-grid-mode-next-N (n)
  "Page N items off the top."
  ;; argh - this is related to ido-cur-list
  ;; that's why it doesn't work
  (setq ido-cur-list (ido-grid-mode-rotate-n n ido-matches ido-cur-list)
        ido-rescan t
        ido-rotate t))

(defun ido-grid-mode-previous-N (n)
  "Page N items off the bottom to the top."
  (setq ido-cur-list (ido-grid-mode-rotate-n (- n) ido-matches ido-cur-list)
        ido-rescan t
        ido-rotate t)

  ;; (when (and ido-matches
  ;;            (< ido-grid-mode-count (length ido-matches)))
  ;;   (let ((shift 0))
  ;;     (while (<= shift ido-grid-mode-count)
  ;;       (ido-prev-match)
  ;;       (cl-incf shift)
  ;;       (ido-grid-mode-completions ""))
  ;;     (ido-next-match)
  ;;     (ido-grid-mode-completions "")))
  )

(defvar ido-grid-mode-old-max-mini-window-height nil)
(defvar ido-grid-mode-old-resize-mini-windows 'unknown)

(defun ido-grid-mode-advise-match-temporary (o &rest args)
  "Advice for things which use `ido-matches' temporarily."
  (let ((ido-matches (nthcdr ido-grid-mode-offset ido-matches))
        (ido-grid-mode-offset 0))
    (apply o args)))

(defun ido-grid-mode-advise-match-permanent (o &rest args)
  "Advice for things which use `ido-matches' permanently"
  (dotimes (_n ido-grid-mode-offset) (ido-next-match))
  (setq ido-grid-mode-offset 0)
  (setq max-mini-window-height (or ido-grid-mode-old-max-mini-window-height max-mini-window-height)
        resize-mini-windows (or (unless (equal ido-grid-mode-old-resize-mini-windows 'unknown)
                                  ido-grid-mode-old-resize-mini-windows)
                                resize-mini-windows)
        ido-grid-mode-old-resize-mini-windows 'unknown
        ido-grid-mode-old-max-mini-window-height 0)
  (apply o args))

(defun ido-grid-mode-advise-functions ()
  "Add advice to functions which need it."
  (dolist (fn ido-grid-mode-advise-perm)
    (advice-add fn :around #'ido-grid-mode-advise-match-permanent))
  (dolist (fn ido-grid-mode-advise-temp)
    (advice-add fn :around #'ido-grid-mode-advise-match-temporary)))

(defun ido-grid-mode-unadvise-functions ()
  "Remove added advice."
  (dolist (fn ido-grid-mode-advise-perm)
    (advice-remove fn #'ido-grid-mode-advise-match-permanent))
  (dolist (fn ido-grid-mode-advise-temp)
    (advice-remove fn #'ido-grid-mode-advise-match-temporary)))

(defun ido-grid-mode-ido-setup ()
  "Setup key bindings, etc."
  (setq ido-grid-mode-offset 0)
  (setq ido-grid-mode-collapsed ido-grid-mode-start-collapsed)
  (setq ido-grid-mode-old-max-mini-window-height max-mini-window-height
        ido-grid-mode-old-resize-mini-windows resize-mini-windows
        resize-mini-windows t
        max-mini-window-height (max max-mini-window-height
                                    (1+ ido-grid-mode-max-rows)))

  (dolist (k ido-grid-mode-keys)
    (cl-case k
      ('tab (setq ido-cannot-complete-command 'ido-grid-mode-tab))
      ('backtab (define-key ido-completion-map (kbd "<backtab>") #'ido-grid-mode-previous))
      ('left    (define-key ido-completion-map (kbd "<left>")    #'ido-grid-mode-left))
      ('right   (define-key ido-completion-map (kbd "<right>")   #'ido-grid-mode-right))
      ('up      (define-key ido-completion-map (kbd "<up>")      #'ido-grid-mode-up))
      ('down    (define-key ido-completion-map (kbd "<down>")    #'ido-grid-mode-down))
      ('C-n     (define-key ido-completion-map (kbd "C-n")       #'ido-grid-mode-next-page))
      ('C-p     (define-key ido-completion-map (kbd "C-p")       #'ido-grid-mode-previous-page))
      )))

;; this could be done with advice - is advice better?
;; I guess this is like advice which definitely ends up at the bottom?
(defvar ido-grid-mode-old-completions nil)
(defvar ido-grid-mode-old-cannot-complete-command nil)

(defun ido-grid-mode-enable ()
  "Turn on ido-grid-mode."
  (setq ido-grid-mode-order
        (cl-case ido-grid-mode-order
          (rows nil)
          (columns t)
          (t ido-grid-mode-order)))
  (setq ido-grid-mode-old-completions (symbol-function 'ido-completions))
  (setq ido-grid-mode-old-cannot-complete-command ido-cannot-complete-command)
  (fset 'ido-completions #'ido-grid-mode-completions)
  (add-hook 'ido-setup-hook #'ido-grid-mode-ido-setup)
  (ido-grid-mode-advise-functions))

(defun ido-grid-mode-disable ()
  "Turn off ido-grid-mode."
  (fset 'ido-completions ido-grid-mode-old-completions)
  (setq ido-cannot-complete-command ido-grid-mode-old-cannot-complete-command)
  (remove-hook 'ido-setup-hook #'ido-grid-mode-ido-setup)
  (ido-grid-mode-unadvise-functions))

;;;###autoload
(define-minor-mode ido-grid-mode
  "Makes ido-mode display candidates in a grid."
  :global t
  :group 'ido-grid-mode
  (if ido-grid-mode
      (ido-grid-mode-enable)
    (ido-grid-mode-disable)))

(provide 'ido-grid-mode)

;;; ido-grid-mode.el ends here
