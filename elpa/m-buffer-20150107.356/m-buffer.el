;;; m-buffer.el --- List-Oriented, Functional Buffer Manipulation -*- lexical-binding: t -*-

;;; Header:

;; This file is not part of Emacs

;; Author: Phillip Lord <phillip.lord@newcastle.ac.uk>
;; Maintainer: Phillip Lord <phillip.lord@newcastle.ac.uk>
;; Version: 0.8
;; Package-Requires: ((dash "2.8.0")(emacs "24.3"))

;; The contents of this file are subject to the GPL License, Version 3.0.

;; Copyright (C) 2014, Phillip Lord, Newcastle University

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file provides a set of list-oriented functions for operating over the
;; contents of buffers. They avoid the use of looping, manipulating global state
;; with `match-data'. Many high-level functions exist for matching sentences,
;; lines and so on.

;; Functions are generally purish: i.e. they may change the state of one
;; buffer by side-effect, but should not affect point, current buffer, match
;; data or so forth. Generally, markers are returned rather than point
;; locations, so that it is possible for example, to search for regexp
;; matches, and then replace them all without the early replacement
;; invalidating the location of the later ones.

;; Other files provide something similar: stateless alternatives to existing
;; emacs functions. `m-buffer-macro' provides macros to operate in the context of
;; a marker, and for marker cleanup. `m-buffer-at' provides functions for
;; operating at point (without using point!).

;; This file is documented using lentic.el.

;;; Status:

;; This library is early release at the moment. I write it become I got fed up
;; with writing (while (re-search-forward) do-stuff) forms. I found that it
;; considerably simplified writing `linked-buffer'. The API is beginning to
;; stablize now and should not undergo major changes.

;;; Code:

;; #+begin_src emacs-lisp

(require 'dash)
(require 'm-buffer-macro)
;; #+end_src

;; ** Regexp Matching

;; This section provides the core functions which convert between Emacs' stateful
;; matching and a more sequence oriented approach.

;; `m-buffer-match' provides the main interface for this.

;; #+begin_src emacs-lisp
(defun m-buffer-match (&rest match)
  "Return a list of all `match-data' for MATCH.
MATCH may be of the forms:
BUFFER REGEXP &optional MATCH-OPTIONS
WINDOW REGEXP &optional MATCH-OPTIONS
MATCH-OPTIONS

If BUFFER is given, search this buffer. If WINDOW is given search
the visible window. MATCH-OPTIONS is a plist with any of the
following keys:
:buffer -- the buffer to search
:regexp -- the regexp to search with
:begin -- the start of the region to search -- default point min
:end -- the end of the region to search -- default point max
:post-match -- function called after a match -- default nil
:widen -- if true, widen buffer first -- default nil
:case-fold-search value of `case-fold-search' during search.
If :default accept the current buffer-local value
:numeric -- if true, return integers not markers

If options are expressed in two places, the plist form takes
precedence over positional args. So calling with both a first
position buffer and a :buffer arg will use the second. Likewise,
if a window is given as first arg and :end is given, then
the :end value will be used.

REGEXP should advance point (i.e. not be zero-width) or the
function will loop infinitely. POST-MATCH can be used to avoid
this. The buffer is searched forward."
  (apply 'm-buffer--match-1
         (m-buffer--normalize-args match)))


;; #+end_src

;; All match functions route through here at some point.

;; #+begin_src emacs-lisp
(defun m-buffer--match-1 (buffer regexp begin end
                                post-match widen cfs
                                numeric)
  "Return a list of `match-data'.

This is an internal function: please prefer `m-buffer-match'.

BUFFER -- the buffer.
REGEXP -- the regexp.
BEGIN -- the start of the region to search
END -- the end of the region to search
POST-MATCH -- function to run after each match
POST-MATCH is useful for zero-width matches which will otherwise
cause infinite loop. The buffer is searched forward. POST-MATCH
return can also be used to terminate the matching by returning nil.
WIDEN -- call widen first.
CFS -- Non-nil if searches and matches should ignore case.
NUMERIC -- Non-nil if we should return integers not markers."
;; #+end_src

;; We start by saving everything to ensure that we do not pollute the global
;; state. This means match-data, point, narrowing and current buffer!

;; #+begin_src emacs-lisp
  (save-match-data
    (save-excursion
      (save-restriction
        (with-current-buffer
            buffer
          (when widen (widen))
;; #+end_src

;; This let form is doing a number of things. It sets up a dynamic binding for
;; `case-fold-search', ensures a non-nil value for =end-bound= and defines a
;; sentinal value that =post-match-return= can use to end early.

;; #+begin_src emacs-lisp
          (let ((rtn nil)
                (post-match-return t)
                (end-bound (or end (point-max)))
                ;; over-ride default if necessary
                (case-fold-search
                 (if (eq :default cfs)
                     case-fold-search
                   cfs)))
;; #+end_src

;; To begin at the beginning.

;; #+begin_src emacs-lisp
            (goto-char
             (or begin
                 (point-min)))
            (while
                (and
;; #+end_src

;; The original purpose for =post-match-return= was for zero-width matches --
;; these do not advance point beyond their end, so the while loop never
;; terminates. Unfortunately, avoiding this depends on the regexp being called,
;; so we provide the most general solution of all.

;; #+begin_src emacs-lisp
                 post-match-return
                 ;; we need to check we are less than the end-bound
                 ;; or re-search-forward will break
                 (<= (point) end-bound)
                 (re-search-forward
                  regexp end-bound
                  t))
;; #+end_src

;; Store the `match-data' in a backward list, run post-match. Finally, reverse
;; and terminate.

;; #+begin_src emacs-lisp
              (setq rtn
                    (cons
                     (if numeric
                         (m-buffer-marker-to-pos-nil
                          (match-data))
                       (match-data))
                     rtn))
              (when post-match
                (setq post-match-return (funcall post-match))))
            (reverse rtn)))))))
;; #+end_src

;; While this is all fairly nasty, it means that I can provide a keyword argument
;; interface to all the functions which follow. At the time of writing, we have
;; eight arguments, most of which are not essential, so this is well worth the
;; effort.

;; #+begin_src emacs-lisp
(defun m-buffer--normalize-args (match-with)
  "Manipulate args into a standard form and return as a list.
MATCH-WITH are these args.This is an internal function."
  (let* (
         ;; split up into keyword and non keyword limits
         (args
          (-take-while
           (lambda (x) (not (keywordp x)))
           match-with))
         (pargs
          (-drop-while
           (lambda (x) (not (keywordp x)))
           match-with))
         ;; sort actual actual parameters
         (first (car args))
         ;; buffer may be first
         (buffer
          (or (plist-get pargs :buffer)
              (and (bufferp first) first)))
         ;; or window may be first
         (window
          (or (plist-get pargs :window)
              (and (windowp first) first)))
         ;; regexp always comes second
         (regexp
          (or (plist-get pargs :regexp)
              (nth 1 args)))
         ;; begin depends on other arguments
         (begin
          (or (plist-get pargs :begin)
              (and window (window-start window))))
         ;; end depends on other arguments
         (end
          (or (plist-get pargs :end)
              (and window (window-end window))))
         ;; pm
         (post-match
          (plist-get pargs :post-match))

         ;; widen
         (widen
           (plist-get pargs :widen))

         ;; case-fold-search this needs to overwrite the buffer contents iff
         ;; set, otherwise be ignore, so we need to distinguish a missing
         ;; property and a nil one
         (cfs
          (if (plist-member pargs :case-fold-search)
              (plist-get pargs :case-fold-search)
            :default))

         ;; numeric
         (numeric
          (plist-get pargs :numeric)))

    (list buffer regexp begin end post-match widen cfs numeric)))
;; #+end_src

;; Finally, this function provides a link between the match function, and the
;; match manipulation functions. We can either choose to match once against a set
;; of arguments and then apply multiple manipulations on the returned match data.
;; Or just use the match manipulation function directly.

;; #+begin_src emacs-lisp

(defun m-buffer-ensure-match (&rest match)
  "Ensure that we have MATCH data.
If a single arg, assume it is match data and return. If multiple
args, assume they are of the form accepted by
`m-buffer-match'."
  (cond
   ;; we have match data
   ((= 1 (length match))
    (car match))
   ((< 1 (length match))
    (apply 'm-buffer-match match))
   (t
    (error "Invalid arguments"))))

;; #+end_src

;; ** Match Data Manipulation Functions

;; These functions take either a list of match-data or arguments for a match,
;; and manipulate it in some way.

;; #+begin_src emacs-lisp
(defun m-buffer-buffer-for-match (match-data)
  "Given some MATCH-DATA return the buffer for that data."
  (marker-buffer (caar match-data)))

(defun m-buffer-match-nth-group (n match-data)
  "Fetch the Nth group from MATCH-DATA."
  (-map
   (lambda (m)
     (let ((drp
            (-drop (* 2 n) m)))
       (list
        (car drp) (cadr drp))))
   match-data))

(defun m-buffer-match-begin-n (n &rest match)
  "Return markers to the start of the Nth group in MATCH.
MATCH may be of any form accepted by `m-buffer-ensure-match'. Use
`m-buffer-nil-marker' after the markers have been finished with
or they will slow future use of the buffer until garbage collected."
  (-map
   (lambda (m)
     (nth
      (* 2 n) m))
   (apply 'm-buffer-ensure-match match)))

(defun m-buffer-match-begin-n-pos (n &rest match)
  "Return positions of the start of the Nth group in MATCH.
MATCH may be of any form accepted by `m-buffer-ensure-match'. If
`match-data' is passed markers will be set to nil after this
function. See `m-buffer-nil-marker' for details."
  (m-buffer-marker-to-pos-nil
   (apply 'm-buffer-match-begin-n
          n match)))

(defun m-buffer-match-begin (&rest match)
  "Return a list of markers to the start of MATCH.
MATCH may of any form accepted by `m-buffer-ensure-match'. Use
`m-buffer-nil-marker' after the markers have been used or they
will slow future changes to the buffer."
  (apply 'm-buffer-match-begin-n 0 match))

(defun m-buffer-match-begin-pos (&rest match)
  "Return a list of positions at the start of matcher.
MATCH may be of any form accepted by `m-buffer-ensure-match'.
If `match-data' is passed markers will be set to nil after this
function. See `m-buffer-nil-marker' for details."
  (apply 'm-buffer-match-begin-n-pos 0 match))

(defun m-buffer-match-end-n (n &rest match)
  "Return markers to the end of the match to the Nth group.
MATCH may be of any form accepted by `m-buffer-ensure-match'.
If `match-data' is passed markers will be set to nil after this
function. See `m-buffer-nil-marker' for details."
  (-map
   (lambda (m)
     (nth
      (+ 1 (* 2 n))
      m))
   (apply 'm-buffer-ensure-match match)))

(defun m-buffer-match-end-n-pos (n &rest match)
  "Return positions of the end Nth group of MATCH.
MATCH may be of any form accepted by `m-buffer-ensure-match'.
If `match-data' is passed markers will be set to nil after this
function. See `m-buffer-nil-marker' for details."
  (m-buffer-marker-to-pos-nil
   (apply 'm-buffer-match-end-n-pos
          n match)))

(defun m-buffer-match-end (&rest match)
  "Return a list of markers to the end of MATCH to regexp in buffer.
MATCH may be of any form accepted by `m-buffer-ensure-match'. Use
`m-buffer-nil-marker' after the markers have been used or they
will slow future changes to the buffer."
  (apply 'm-buffer-match-end-n 0 match))

(defun m-buffer-match-end-pos (&rest match)
  "Return a list of positions to the end of the match.
MATCH may be of any form accepted by `m-buffer-ensure-match'.
If `match-data' is passed markers will be set to nil after this
function. See `m-buffer-nil-marker' for details."
  (m-buffer-marker-to-pos-nil
   (apply 'm-buffer-match-end match)))
;; #+end_src

;; ** Match Utility and Predicates

;; Some predicates and the ability to subtract to lists of matches from each
;; other. This makes up for limitations in Emacs regexp which can't do "match x
;; but not y".

;; #+begin_src emacs-lisp
(defun m-buffer-match-equal (m n)
  "Return true if M and N are cover the same region.
Matches are equal if they match the same region; subgroups are
ignored."
  ;; can we speed this up by not making subsets?
  (and
   (equal
    (car m)
    (car n))
   (equal
    (cadr m)
    (cadr n))))

(defun m-buffer-match-subtract (m n)
  "Remove from M any match in N.
Matches are equivalent if overall they match the same
area; subgroups are ignored.
See also `m-buffer-match-exact-subtract' which often
runs faster but has some restrictions."
  (-remove
   (lambda (o)
     (-any?
      (lambda (p)
        (m-buffer-match-equal o p))
      n))
   m))

(defun m-buffer-match-exact-subtract (m n)
  "Remove from M any match in N.
Both M and N must be fully ordered, and any element in N must be
in M."
  (if n
      ;; n-eaten contains the remaining elements of n that we haven't tested
      ;; for yet. We throw them away as we go
      (let ((n-eaten n))
        (-remove
         (lambda (o)
           (cond
            ;; n-eaten has been eaten. Check here or later "<" comparison crashes.
            ((not n-eaten)
             ;; return nil because we always want things in m now.
             nil
             )
            ;; we have a match so throw away the first element of n-eaten
            ;; which we won't need again.
            ((m-buffer-match-equal
              (car n-eaten) o)
             (progn
               (setq n-eaten (-drop 1 n-eaten))
               t))
            ;; we should discard also if n-eaten 1 is less than o because, both
            ;; are sorted, so we will never match
            ((<
              ;; first half of the first match in n-eaten
              (caar n-eaten)
              ;; first half of match
              (car o))
             (progn
               (setq n-eaten (-drop 1 n-eaten))
               t))))
         m))
    m))

(defun m-buffer-in-match-p (matches position)
  "Returns true is any of MATCHES contain POSITION."
  (-any?
   (lambda (match)
     (and
      (<= (car match) position)
      (<= position (cadr match))))
   matches))

;; #+end_src

;; ** Marker Position Utility

;; Functions for transforming between markers and positions, and 
;; for niling markers, to prevent buffer slow down before GC.

;; #+begin_src emacs-lisp

(defun m-buffer-nil-marker (markers)
  "Takes a (nested) list of MARKERS and nils them all.
Markers slow buffer movement while they are pointing at a
specific location, until they have been garbage collected. Niling
them prevents this. See Info node `(elisp) Overview of Markers'."
  (-map
   (lambda (marker)
     (set-marker marker nil))
   (-flatten markers)))

(defun m-buffer-marker-to-pos (markers &optional postnil)
  "Transforms a list of MARKERS to a list of positions.
If the markers are no longer needed, set POSTNIL to true, or call
`m-buffer-nil-marker' manually after use to speed future buffer
movement. Or use `m-buffer-marker-to-pos-nil'."
  (-map
   (lambda (marker)
     (prog1
         (marker-position marker)
       (when postnil
         (set-marker marker nil))))
   markers))

(defun m-buffer-marker-to-pos-nil (markers)
  "Transforms a list of MARKERS to a list of positions then nils.
See also `m-buffer-nil-markers'"
  (m-buffer-marker-to-pos markers t))

(defun m-buffer-marker-tree-to-pos (marker-tree &optional postnil)
  "Transforms a tree of markers to equivalent positions.
MARKER-TREE is the tree.
POSTNIL sets markers to nil afterwards."
  (-tree-map
   (lambda (marker)
     (prog1
         (marker-position marker)
       (when postnil
         (set-marker marker nil))))
   marker-tree))

(defun m-buffer-marker-tree-to-pos-nil (marker-tree)
  "Transforms a tree of markers to equivalent positions.
MARKER-TREE is the tree. Markers are niled afterwards."
  (m-buffer-marker-tree-to-pos marker-tree t))

(defun m-buffer-marker-clone (marker-tree &optional type)
  "Return a clone of MARKER-TREE.
The optional argument TYPE specifies the insertion type. See
`copy-marker' for details."
  (-tree-map
   (lambda (marker)
     (copy-marker marker type))
   marker-tree))

(defun m-buffer-pos-to-marker (buffer positions)
  "In BUFFER translates a list of POSITIONS to markers."
  (-map
   (lambda (pos)
     (set-marker
      (make-marker) pos buffer))
   positions))
;; #+end_src

;; ** Replace, Delete, Extract

;; #+begin_src emacs-lisp
(defun m-buffer-replace-match (match-data replacement
                                          &optional fixedcase literal subexp)
  "Given a list of MATCH-DATA, replace with REPLACEMENT.
If FIXEDCASE do not alter the case of the replacement text.
If LITERAL insert the replacement literally.
SUBEXP should be a number indicating the regexp group to replace.
Returns markers to the start and end of the replacement. These
markers are part of MATCH-DATA, so niling them will percolate backward.

See also `replace-match'."
  (-map
   (lambda (match)
     (with-current-buffer
         (marker-buffer (car match))
       (save-match-data
         (set-match-data match)
         (replace-match
          replacement fixedcase literal nil
          (or subexp 0)))))
   match-data)
  ;; we have match-data
  (m-buffer-match-nth-group (or subexp 0) match-data))

(defun m-buffer-delete-match (match-data &optional subexp)
  "Delete all MATCH-DATA.
SUBEXP should be a number indicating the regexp group to delete.
Returns markers to the start and end of the replacement. These
markers are part of MATCH_DATA, so niling them will percolate backward."
  (m-buffer-replace-match match-data "" subexp))

(defun m-buffer-match-string (match-data &optional subexp)
  "Return strings for MATCH-DATA optionally of group SUBEXP."
  (-map
   (lambda (match)
     (with-current-buffer
         (marker-buffer (car match))
       (save-match-data
         (set-match-data match)
         (match-string
          (or subexp 0)))))
   match-data))

(defun m-buffer-match-string-no-properties (match-data &optional subexp)
  "Return strings for MATCH-DATA optionally of group SUBEXP.
Remove all properties from return."
  (-map
   'substring-no-properties
   (m-buffer-match-string
    match-data subexp)))

;; #+end_src

;; ** Match Things

;; These functions provide a set of pre-defined args for matching common
;; entities.


;; #+begin_src emacs-lisp
(defun m-buffer-apply-snoc (fn list &rest element)
  "Apply FN to LIST and all ELEMENT."
  (apply
   fn (append list element)))

(defun m-buffer-match-page (&rest match)
  "Return a list of match data to all pages in MATCH.
MATCH is of form BUFFER-OR-WINDOW MATCH-OPTIONS. See
`m-buffer-match' for further details."
  (m-buffer-apply-snoc 'm-buffer-match
                       match :regexp page-delimiter))

(defun m-buffer-match-paragraph-separate (&rest match)
  "Return a list of match data to `paragraph-separate' in MATCH.
MATCH is of form BUFFER-OR-WINDOW MATCH-OPTIONS. See
`m-buffer-match' for futher details."
  (m-buffer-apply-snoc
   'm-buffer-match match :regexp paragraph-separate
   :post-match 'm-buffer-post-match-forward-line))

(defvar m-buffer--line-regexp
  "^.*$"
  "Regexp to match a line.")

(defun m-buffer-match-line (&rest match)
  "Return a list of match data to all lines.
MATCH is of the form BUFFER-OR-WINDOW MATCH-OPTIONS.
See `m-buffer-match for further details."
  (m-buffer-apply-snoc
   'm-buffer-match
   match :regexp m-buffer--line-regexp
   :post-match 'm-buffer-post-match-forward-char))

(defun m-buffer-match-line-start (&rest match)
  "Return a list of match data to all line start.
MATCH is of form BUFFER-OR-WINDOW MATCH-OPTIONS. See
`m-buffer-match' for further details."
  (m-buffer-apply-snoc
   'm-buffer-match-begin
   match :regexp  "^"
   :post-match 'm-buffer-post-match-forward-char))

(defun m-buffer-match-line-end (&rest match)
  "Return a list of match to line end.
MATCH is of form BUFFER-OR-WINDOW MATCH-OPTIONS. See
`m-buffer-match' for further details."
  (m-buffer-apply-snoc
   'm-buffer-match-begin
   match :regexp "$"
   :post-match 'm-buffer-post-match-forward-char))

(defun m-buffer-match-first-line (&rest match)
  "Returns a match to the first line of MATCH.
This matches more efficiently than matching all lines and taking
the car. See `m-buffer-match' for further details of MATCH."
  (m-buffer-apply-snoc
   'm-buffer-match match
   :regexp m-buffer--line-regexp
   :post-match (lambda () nil)))

(defun m-buffer-match-sentence-end (&rest match)
  "Return a list of match to sentence end.
MATCH is of the form BUFFER-OR-WINDOW MATCH-OPTIONS. See
`m-buffer-match' for further details."
  (m-buffer-apply-snoc
   'm-buffer-match-begin
   match :regexp (sentence-end)))

(defun m-buffer-match-word (&rest match)
  "Return a list of match to all words.
MATCH is of the form BUFFER-OR-WINDOW MATCH-OPTIONS. See
`m-buffer-match' for further details."
  (m-buffer-apply-snoc
   'm-buffer-match
   match :regexp "\\\w+"))

(defun m-buffer-match-empty-line (&rest match)
  "Return a list of match to all empty lines.
MATCH is of the form BUFFER-OR-WINDOW MATCH-OPTIONS. See
`m-buffer-match' for further details."
  (m-buffer-apply-snoc
   'm-buffer-match
   match :regexp "^$"
   :post-match 'm-buffer-post-match-forward-line))

(defun m-buffer-match-non-empty-line (&rest match)
  "Return a list of match to all non-empty lines.
MATCH is fo the form BUFFER-OR-WINDOW MATCH-OPTIONS. See
`m-buffer-match' for further details."
  (m-buffer-apply-snoc
   'm-buffer-match
   match :regexp "^.+$"))

(defun m-buffer-match-whitespace-line (&rest match)
  "Return match data to all lines with only whitespace characters.
Note empty lines are not included. MATCH is of form
BUFFER-OR-WINDOW MATCH-OPTIONS. See `m-buffer-match' for
further details."
  (m-buffer-apply-snoc
   'm-buffer-match
   match :regexp "^\\s-+$"))

(defun m-buffer-match-non-whitespace-line (&rest match)
  "Return match data to all lines with at least one non-whitespace character.
Note empty lines do not contain any non-whitespace lines.
MATCH is of form BUFFER-OR-WINDOW MATCH-OPTIONS. See
`m-buffer-match' for further details."
  (-difference
   (apply 'm-buffer-match-line match)
   (apply 'm-buffer-match-whitespace-line match)))

;; Useful post-match functions
(defun m-buffer-post-match-forward-line ()
  "Attempt to move forward one line, return true if success."
  (= 0 (forward-line)))

(defun m-buffer-post-match-forward-char ()
  "Attempts to move forward one char.
Returns true if succeeds."
  (condition-case e
      (progn
        (forward-char)
        t)
    (error 'end-of-buffer
           nil)))


;; #+end_src


;; ** Apply Function to Match

;; These functions call a function to some match data.

;; #+begin_src emacs-lisp
(defun m-buffer-on-region (fn match-data)
  "Apply FN to MATCH-DATA.
FN should take two args, the start and stop of each region.
MATCH-DATA can be any list of lists with two elements (or more)."
  (m-buffer-on-region-nth-group fn 0 match-data))

(defun m-buffer-on-region-nth-group (fn n match-data)
  "Apply FN to the Nth group of MATCH-DATA.
FN should take two args, the start and stop of each region.
MATCH-DATA can be any list of lists with two elements (or more)."
  (-map
   (lambda (x)
     (apply fn x))
   (m-buffer-match-nth-group n match-data)))

;; #+end_src

;; ** Overlay and Property Functions

;; Add properties or overlays to matches

;; #+begin_src emacs-lisp
(defun m-buffer-overlay-match (match-data &optional front-advance rear-advance)
  "Return an overlay for all match to MATCH-DATA.
FRONT-ADVANCE and REAR-ADVANCE controls the borders of the
overlay as defined in `make-overlay'. Overlays do not scale that
well, so use `m-buffer-propertize-match' if you intend to make
and keep many of these.

See Info node `(elisp) Overlays' for further information."
  (let ((buffer (m-buffer-buffer-for-match match-data)))
    (m-buffer-on-region
     (lambda (beginning end)
       (make-overlay
        beginning end buffer
        front-advance rear-advance))
     match-data)))

(defun m-buffer-add-text-property-match
  (match-data properties)
  "To MATCH-DATA add PROPERTIES.
See `add-text-property' for details of the format of properties.
Text properties are associated with the text and move with it. See
Info node `(elisp) Text Properties' for further details."
  (let ((buffer (m-buffer-buffer-for-match match-data)))
    (m-buffer-on-region
     (lambda (beginning end)
       (add-text-properties beginning end properties))
     match-data)))

(defun m-buffer-put-text-property-match (match-data property value)
  "To MATCH-DATA add PROPERTY wth VALUE.
See `put-text-property' for details of the format of properties.
Text properties are associated with the text and move with it. See
Info node `(elisp) Text Properties' for further details."
  (let ((buffer (m-buffer-buffer-for-match match-data)))
    (m-buffer-on-region
     (lambda (beginning end)
       (put-text-property beginning end property value))
     match-data)))

(defun m-buffer-overlay-face-match (match-data face)
  "To MATCH-DATA add FACE to the face property.
This is for use in buffers which do not have function `font-lock-mode'
enabled; otherwise use `m-buffer-overlay-font-lock-face-match'."
  (-map
   (lambda (ovly)
     (overlay-put ovly 'face face))
   (m-buffer-overlay-match match-data)))

(defun m-buffer-overlay-font-lock-face-match (match-data face)
  "To MATCH-DATA add FACE to the face property.
This is for use in buffers which have variable `font-lock-mode' enabled;
otherwise use `m-buffer-overlay-face-match'."
  (-map
   (lambda (ovly)
     (overlay-put ovly 'face face))
   (m-buffer-overlay-match match-data)))

(defun m-buffer-text-property-face (match-data face)
  "To MATCH-DATA apply FACE.
This is for use in buffers which do
not have variable `font-lock-mode' enabled; otherwise use
`m-buffer-text-property-font-lock-face'."
  (m-buffer-put-text-property-match match-data
   'face face))

(defun m-buffer-text-property-font-lock-face (match-data face)
  "To MATCH-DATA apply FACE.
This is for use in buffers which have variable `font-lock-mode'
enabled; otherwise use `m-buffer-text-property-face'."
  (m-buffer-put-text-property-match match-data
   'font-lock-face face))

(provide 'm-buffer)


;;; m-buffer.el ends here
;; #+end_src
