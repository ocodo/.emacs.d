;;; multi-line-cycle.el --- multi-line statements -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2016 Ivan Malison

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

;; multi-line-cycle defines a respacer object that dispatch to various
;; other respacers based on context and the previous user action.

;;; Code:

(require 'eieio)

(require 'multi-line-candidate)

;; This variable is for internal use only
(defvar multi-line-last-cycler nil)

(defclass multi-line-cycle-respacer ()
  ((respacers :initarg :respacers)
   (named-respacers :initarg :named-respacers :initform nil)
   (last-cycle-marker :initform nil)
   (cycle-index :initform 0)
   (command-at-last-cycle :initform nil)
   (check-markers :initform t :initarg check-markers)
   (check-last-command :initform nil :initarg check-last-command)))

(defmethod multi-line-respace ((cycler multi-line-cycle-respacer) candidates
                               &optional context)
  (let* ((respacer-name (plist-get context :respacer-name))
         (respacer-index (plist-get context :respacer-index))
         (respacer
          (or (plist-get (oref cycler named-respacers)
                         respacer-name)
              (when respacer-index
                (nth respacer-index (oref cycler respacers))))))
    (if respacer
        ;; We want to reset in this case because something has been selected
        ;; explicitly.
        (multi-line-cycler-reset cycler nil)
      ;; Pass in the first marker that was found so that we get consistent
      ;; cycling behavior even if the cursor is moved.
      (setq respacer (multi-line-cycle cycler (oref (car candidates) marker))))
    (multi-line-respace respacer candidates context)))

(defmethod multi-line-cycle ((cycler multi-line-cycle-respacer) current-marker)
  (if (and (eq multi-line-last-cycler cycler)
           (or (not (oref cycler check-last-command))
               (equal (oref cycler command-at-last-cycle) last-command))
           (or (not (oref cycler check-markers))
               (let ((last-marker (oref cycler last-cycle-marker)))
                 (equal current-marker last-marker))))
      (multi-line-increment-cycle-index cycler)
    (multi-line-cycler-reset cycler current-marker))
  (multi-line-current-respacer cycler))

(defmethod multi-line-current-respacer ((cycler multi-line-cycle-respacer))
  (nth (oref cycler cycle-index) (oref cycler respacers)))

(defmethod multi-line-cycler-reset ((cycler multi-line-cycle-respacer) current-marker)
  (oset cycler last-cycle-marker current-marker)
  (oset cycler cycle-index 0)
  (oset cycler command-at-last-cycle this-command)
  (setq multi-line-last-cycler cycler))

(defmethod multi-line-increment-cycle-index ((cycler multi-line-cycle-respacer)
                                             &optional amount)
  (unless amount (setq amount 1))
  (oset cycler cycle-index
        (% (+ (oref cycler cycle-index) amount)
           (length (oref cycler respacers)))))

(defun multi-line-build-from-respacers-list (respacers-list)
  (let* ((named-respacers nil)
         (respacers
          (cl-loop for respacer-spec in respacers-list
                   collect (pcase respacer-spec
                             (`(,name . ,respacer)
                              (setq named-respacers
                                    (plist-put named-respacers name respacer))
                              respacer)
                             (_ respacer-spec)))))
    (make-instance 'multi-line-cycle-respacer :respacers respacers
                   :named-respacers named-respacers)))

(provide 'multi-line-cycle)
;;; multi-line-cycle.el ends here
