;;; libmpdel-directory.el --- Handling directories  -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2021  Damien Cassou

;; Author: Jose A Ortega <jao@gnu.org>
;; Keywords: multimedia
;; Url: https://gitea.petton.fr/mpdel/libmpdel
;; Package-requires: ((emacs "25.1"))
;; Version: 1.2.0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This module implements the directory datatype, used to traverse
;; local or virtual directories (collections) in MPD servers.

;;; Code:
(require 'libmpdel)
(require 'cl-lib)


(cl-defstruct (libmpdel-directory
               (:constructor libmpdel--directory-create)
               (:conc-name libmpdel--directory-))
  (path nil :read-only t)
  (name nil :read-only t))

(cl-defmethod libmpdel-entity-name ((_entity (eql directories)))
  "The name for the `directories' entity."
  "Directories")

(cl-defmethod libmpdel-entity-to-criteria ((_entity (eql directories)))
  "Return a query to retrieve the list of directories from the server."
  "lsinfo \"\"")

(defun libmpdel-directory--create-directories-from-data (data)
  "Return a list of directories extracted from DATA returned by MPD."
  (mapcar (lambda (dir-name) (libmpdel--directory-create :path dir-name))
          (libmpdel-sorted-entries data 'directory)))

(cl-defmethod libmpdel-list ((_entity (eql directories)) function)
  "Call FUNCTION with a list of directories as argument."
  (libmpdel-send-command
   (libmpdel-entity-to-criteria 'directories)
   (lambda (data)
     (funcall function
              (libmpdel-directory--create-directories-from-data data)))))

(cl-defmethod libmpdel-entity-name ((dir libmpdel-directory))
  "Return DIR's entity name."
  (or (libmpdel--directory-name dir)
      (file-name-nondirectory (or (libmpdel--directory-path dir) ""))))

(cl-defmethod libmpdel-entity-id ((dir libmpdel-directory))
  "Return DIR's entity identifier."
  (libmpdel--directory-path dir))

(eval-when-compile
  (declare-function dired-jump "dired-x"))

(cl-defmethod libmpdel-dired ((dir libmpdel-directory))
  "Jump, using dired, to DIR's local directory."
  (require 'dired-x)
  (dired-jump t (expand-file-name (libmpdel--directory-path dir)
                                  libmpdel-music-directory)))

(cl-defmethod libmpdel-entity-parent ((dir libmpdel-directory))
  "Return the directory containing DIR, or 'directories."
  (let ((path (file-name-directory (libmpdel--directory-path dir))))
    (if (> (length path) 1)
        (libmpdel--directory-create :path (substring path 0 -1))
      'directories)))

(cl-defmethod libmpdel-entity-to-criteria ((dir libmpdel-directory))
  "Return a search criteria to list the contents of DIR, as a string."
  (format "lsinfo %S" (libmpdel--directory-path dir)))

(defun libmpdel-directory--make-dots (dir)
  "Return a list of the . and .. entries associated with the given DIR."
  (list
   (libmpdel--directory-create :path (libmpdel--directory-path dir) :name ".")
   (let ((parent (libmpdel-entity-parent dir)))
     (if (libmpdel-directory-p parent)
         (libmpdel--directory-create :path (libmpdel--directory-path parent)
                                     :name "..")
       parent))))

(cl-defmethod libmpdel-list ((dir libmpdel-directory) function)
  "Call FUNCTION with all the entities contained in DIR."
  (libmpdel-send-command
   (libmpdel-entity-to-criteria dir)
   (lambda (data)
     (let* ((dirs (libmpdel-directory--create-directories-from-data data))
            (songs (libmpdel--create-songs-from-data data))
            (songs (cl-remove-if-not 'libmpdel-entity-name songs))
            (all (append (libmpdel-directory--make-dots dir) dirs songs)))
       (funcall function all)))))

(cl-defmethod libmpdel-playlist-add ((dir libmpdel-directory) playlist)
  "Add the entities contained in DIR (that is, its children) to PLAYLIST."
  (libmpdel-list dir (lambda (children)
                       (libmpdel-playlist-add (cddr children) playlist))))

(provide 'libmpdel-directory)
;;; libmpdel-directory.el ends here
