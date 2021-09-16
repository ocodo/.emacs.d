;;; mpdel-browser.el --- Browsing MPD entities  -*- lexical-binding: t; -*-

;; Copyright (c) 2019, 2020 Jose A Ortega Ruiz

;; Author: Jose A Ortega Ruiz <jao@gnu.org>
;; Keywords: multimedia
;; Url: https://gitea.petton.fr/mpdel/mpdel
;; Package-requires: ((emacs "25.1"))
;; Version: 1.0.0

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

;; Executing `mpdel-browser-open' opens the entities browser, letting
;; users to navigate local directories or virtual collections such as
;; playlists offered by the MPD server, as well as search and jump to
;; the "all artists" and "all albums" colections.

;;; Code:

(require 'libmpdel-directory)
(require 'mpdel-core)
(require 'mpdel-tablist)
(require 'mpdel-playlist)


;;; Customization

(defgroup mpdel-browser nil
  "Display and navigate listings of MPD entities."
  :group 'libmpdel)

(defface mpdel-browser-directory-face
  '((t . (:inherit italic)))
  "Face for directories in browser.")

(defcustom mpdel-browser-top-level-entries
  '(directories empty-line
    albums artists empty-line
    stored-playlists current-playlist empty-line
    search-album search-title search-artist search-filter)
  "A list of the entries to show in the browser's top level buffer.

Each entry is shown as a selectable line with the entry's
description; selecting it with the keyboard or mouse will list
its contents in a new buffer."
  :type '(repeat (choice (const :tag "Directories" directories)
                         (string :tag "Directory")
                         (const :tag "All albums" albums)
                         (const :tag "All artists" artists)
                         (const :tag "Stored playlists" stored-playlists)
                         (const :tag "Current playlist" current-playlist)
                         (const :tag "Search by artist" search-artist)
                         (const :tag "Search by album" search-album)
                         (const :tag "Search by title" search-title)
                         (const :tag "Search using filter" search-filter)
                         (const :tag "Separator" empty-line))))

(defcustom mpdel-browser-list-clean-up-function #'identity
  "Function called with the list of entries to be displayed, for clean-up.

The function is called with the list of retreived entries, and
should return a new list of entries, possibly modified and
re-ordered.  Use cases include elimination of duplicates (some
backends accumulate renamed songs in their listings) or custom
orderings."
  :type 'function)


;;; Formatting of directories in a tablist

(defvar mpdel-browser--song-format (vector (list "Directory / Title" 30 t)
                                           (list "#" 6 nil)
                                           (list "Album" 30 t)
                                           (list "Disk" 4 t)
                                           (list "Date" 5 t)
                                           (list "Artist" 0 t)))

(defun mpdel-browser--directory-format (parent)
  "Return the navigel column format for a directory that is a child of PARENT."
  (vector (list (or (libmpdel--directory-path parent) "") 60 t)))

(defvar mpdel-browser--retrieving-format (vector (list "Retrieving ..." 60 t))
  "Format of the tablist before the children of an entity are known.")

(defun mpdel-browser--includes-songs-p (children)
  "Check whether there is any song among CHILDREN."
  (cl-some #'libmpdel-song-p children))

(defun mpdel-browser--format (parent-directory children)
  "Return format for a directory, given its PARENT-DIRECTORY and its CHILDREN."
  (if (mpdel-browser--includes-songs-p children)
      mpdel-browser--song-format
    (mpdel-browser--directory-format parent-directory)))

(defun mpdel-browser--directory-columns (directory)
  "Return a column for DIRECTORY containing its name."
  (vector (propertize (or directory "") 'face 'mpdel-browser-directory-face)
          "" "" "" "" ""))

(navigel-method mpdel navigel-tablist-format-children ((directory libmpdel-directory) children)
  (mpdel-browser--format directory children))

(navigel-method mpdel navigel-tablist-format-children ((_e (eql directories)) _c)
  (vector (list "Directories" 60 t)))

(navigel-method mpdel navigel-tablist-format ((_e libmpdel-directory))
  mpdel-browser--retrieving-format)

(navigel-method mpdel navigel-tablist-format ((_e (eql directories)))
  mpdel-browser--retrieving-format)

(navigel-method mpdel navigel-entity-to-columns ((directory libmpdel-directory))
  (mpdel-browser--directory-columns (libmpdel-entity-name directory)))

(navigel-method mpdel navigel-entity-to-columns ((_e (eql directories)))
  (vector "Music directory"))


;;; Browser buffers

(defun mpdel-browser--buffer-name (entity)
  "Return the name of a browser buffer displaying ENTITY."
  (format "* %s *"
          (cond ((stringp entity) entity)
                ((libmpdel-directory-p entity)
                 (file-name-nondirectory (or (libmpdel--directory-path entity) "")))
                (t (libmpdel-entity-name entity)))))

(navigel-method mpdel navigel-buffer-name ((_e (eql directories)))
  (mpdel-browser--buffer-name 'directories))

(navigel-method mpdel navigel-buffer-name ((entity libmpdel-directory))
  (mpdel-browser--buffer-name entity))

(navigel-method mpdel navigel-entity-buffer ((_e (eql directories)))
  (mpdel-browser--buffer-name 'directories))

(navigel-method mpdel navigel-entity-buffer ((entity libmpdel-directory))
  (mpdel-browser--buffer-name entity))

(navigel-method mpdel navigel-children
                (entity callback &context (major-mode mpdel-browser-mode))
  (libmpdel-list entity
                 (lambda (c)
                   (funcall callback
                            (funcall mpdel-browser-list-clean-up-function c)))))


;;; Browser top level
(cl-defmethod libmpdel-entity-name ((_e (eql empty-line)))
  "The empty line has an empty name."
  "")

(navigel-method mpdel navigel-open ((_e (eql empty-line)) _t)
  nil)

(defmacro mpdel-browser--defsearch (thing)
  "An utility macro for defining methods associated with a search for THING."
  (let* ((entity (intern (format "search-%s" thing)))
         (name (format "Search by %s" thing))
         (prompt (format "%s: " name))
         (type (symbol-name thing)))
    `(progn
       (cl-defmethod libmpdel-entity-name ((_e (eql ,entity))) ,name)
       (navigel-method mpdel navigel-open ((_e (eql ,entity)) _t)
         (let ((what (read-from-minibuffer ,prompt)))
           (navigel-open (libmpdel-search-criteria-create :type ,type
                                                          :what what)
                         nil))))))

(mpdel-browser--defsearch album)
(mpdel-browser--defsearch artist)
(mpdel-browser--defsearch title)
(mpdel-browser--defsearch filter)

(cl-defmethod libmpdel-entity-name ((path string))
  path)

(navigel-method mpdel navigel-open ((path string) target)
  (let ((navigel-app 'mpdel))
    (navigel-open (libmpdel--directory-create :path path) target)))

(cl-defmethod libmpdel-entity-name ((_e (eql browser)))
  "The name of the top level browser entity."
  "Browser")

(cl-defmethod libmpdel-list ((_e (eql browser)) callback)
  "Listing of the top level browser, passed to CALLBACK.

This listing is constructed using `mpdel-browser-top-level-entries'."
  (funcall callback mpdel-browser-top-level-entries))

(cl-defmethod libmpdel-entity-parent ((_e (eql directories)))
  "The new parent of directories is the browser."
  'browser)

(navigel-method mpdel navigel-buffer-name ((_e (eql browser)))
  (format "* MPDel - %s:%d *" libmpdel-hostname libmpdel-port))

(navigel-method mpdel navigel-entity-buffer ((_e (eql browser)))
  (navigel-buffer-name 'browser))

(navigel-method mpdel navigel-tablist-format ((_e (eql browser)))
  (vector (list "MPDel Browser"  60 t)))

(navigel-method mpdel navigel-entity-to-columns ((_e (eql browser)))
  (vector "Top level"))

(navigel-method mpdel navigel-parent ((_e (eql artists))) 'browser)

(navigel-method mpdel navigel-parent ((_e (eql albums))) 'browser)

(navigel-method mpdel navigel-parent ((_e (eql directories))) 'browser)

(navigel-method mpdel navigel-parent ((_e (eql stored-playlists))) 'browser)

(navigel-method mpdel navigel-parent ((_e (eql current-playlist))) 'browser)

(navigel-method mpdel navigel-children ((_e (eql directories)) callback)
  (libmpdel-list 'directories
                 (lambda (children)
                   (funcall callback (cons 'browser children)))))

;;;###autoload
(defun mpdel-browser-open ()
  "Open the top level MPDel browser buffer."
  (interactive)
  (mpdel-core-open 'browser))


;;; Major mode

(define-derived-mode mpdel-browser-mode mpdel-tablist-mode "MPDel Browser"
  "Mode for browsing directories and their contents")

(navigel-method mpdel navigel-entity-tablist-mode ((_e (eql browser)))
  (mpdel-browser-mode))

(navigel-method mpdel navigel-entity-tablist-mode ((_e (eql directories)))
  (mpdel-browser-mode))

(navigel-method mpdel navigel-entity-tablist-mode ((_e libmpdel-directory))
  (mpdel-browser-mode))

(navigel-method mpdel navigel-entity-tablist-mode ((_e (eql stored-playlists)))
  (mpdel-browser-mode))

(navigel-method mpdel navigel-delete ((stored-playlists list) &context (major-mode mpdel-browser-mode) &optional _callback)
  (libmpdel-stored-playlists-delete stored-playlists))

(cl-defmethod navigel-parent-to-open (_e &context (major-mode mpdel-browser-mode))
  "Find parent of ENTITY when in a buffer with MAJOR-MODE `mpdel-browser-mode'."
  (list (or (navigel-parent navigel-entity) 'browser)))

(cl-defmethod navigel-parent-to-open
  (_e &context (major-mode mpdel-playlist-current-playlist-mode))
  "Find parent of ENTITY when in a buffer with MAJOR-MODE `mpdel-playlist-current-playlist-mode'."
  '(browser . current-playlist))

(define-key mpdel-core-map (kbd ":") #'mpdel-browser-open)


(provide 'mpdel-browser)
;;; mpdel-browser.el ends here
