;;; fifo-class.el --- First in first out abstract class  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2015-2016 Mola-T
;; Author: Mola-T <Mola@molamola.xyz>
;; URL: https://github.com/mola-T/fifo-class
;; Package-Version: 20160425.558
;; Version: 1.0
;; Keywords: lisp
;;
;;; License:
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.
;;
;;; Commentary:
;;
;; Provides first in first out quene for class slot.
;;

;;; Code:
(require 'eieio)

(defclass fifo-class ()
  ()
  "Inherit this class to get first in first out slot."
  :abstract t)

(defmethod fifo-class-push ((obj fifo-class) slot data)

  "First in first out push.
Push DATA to the back of quene of slot SLOT in object OBJ."

  (let ((original (eieio-oref obj slot)))
    (unless (listp original)
      (signal 'wrong-type-argument
              (list 'listp 'obj: (eieio-object-class obj) 'slot: slot)))
    (if original
        (nconc original (list data))
      (setf (eieio-oref obj slot) (list data)))
    t))

(defmethod fifo-class-pop ((obj fifo-class) slot)

  "First in first out pop.
Remove the first element of slot SLOT in object OBJ."
  
    (unless (listp (eieio-oref obj slot))
      (signal 'wrong-type-argument
              (list 'listp 'obj: (eieio-object-class obj) 'slot: slot)))
    (let ((return (car (eieio-oref obj slot))))
      (setf (eieio-oref obj slot) (cdr (eieio-oref obj slot)))
      return))

(defmethod fifo-class-first ((obj fifo-class) slot)

  "Get the first element of the SLOT without removing it."
  
  (unless (listp (eieio-oref obj slot))
      (signal 'wrong-type-argument
              (list 'listp 'obj: (eieio-object-class obj) 'slot: slot)))
  (car (eieio-oref obj slot)))

(provide 'fifo-class)
;;; fifo-class.el ends here
