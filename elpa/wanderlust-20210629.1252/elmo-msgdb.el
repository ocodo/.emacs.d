;;; elmo-msgdb.el --- Message Database for ELMO.  -*- lexical-binding: t -*-

;; Copyright (C) 1998,1999,2000 Yuuichi Teranishi <teranisi@gohome.org>
;; Copyright (C) 2000           Masahiro MURATA <muse@ba2.so-net.ne.jp>

;; Author: Yuuichi Teranishi <teranisi@gohome.org>
;;	Masahiro MURATA <muse@ba2.so-net.ne.jp>
;; Keywords: mail, net news

;; This file is part of ELMO (Elisp Library for Message Orchestration).

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;

;;; Commentary:
;;

;;; Code:
;;

(require 'elmo-vars)
(require 'elmo-util)
(require 'std11)
(require 'mime)
(require 'modb)

;;; MSGDB interface.
;;
;; MSGDB elmo-load-msgdb PATH MIME-CHARSET
;; MSGDB elmo-make-msgdb LOCATION TYPE
;; elmo-msgdb-sort-by-date MSGDB

;; elmo-flag-table-load
;; elmo-flag-table-set
;; elmo-flag-table-get
;; elmo-flag-table-save

;; elmo-msgdb-overview-save DIR OBJ

;;; Abolish
;; elmo-msgdb-get-parent-entity ENTITY MSGDB

;; elmo-msgdb-killed-list-load DIR
;; elmo-msgdb-killed-list-save DIR
;; elmo-msgdb-append-to-killed-list FOLDER MSG
;; elmo-msgdb-killed-list-length KILLED-LIST
;; elmo-msgdb-max-of-killed KILLED-LIST
;; elmo-msgdb-killed-message-p KILLED-LIST MSG
;; elmo-living-messages MESSAGES KILLED-LIST

;; elmo-msgdb-finfo-load
;; elmo-msgdb-finfo-save
;; elmo-msgdb-flist-load
;; elmo-msgdb-flist-save

;; elmo-crosspost-alist-load
;; elmo-crosspost-alist-save

(defconst elmo-msgdb-load-priorities '(legacy standard)
  "Priority list of modb type for load.")

;;; Helper functions for MSGDB
;;
(defun elmo-load-msgdb (location mime-charset)
  "Load the MSGDB from PATH."
  (let ((msgdb (elmo-make-msgdb location elmo-msgdb-default-type mime-charset))
	priorities loaded temp-modb)
    (unless (elmo-msgdb-load msgdb)
      (setq priorities
	    (delq elmo-msgdb-default-type
		  (copy-sequence elmo-msgdb-load-priorities)))
      (while (and priorities
		  (not loaded))
	(setq temp-modb (elmo-make-msgdb location
					 (car priorities)
					 mime-charset)
	      loaded (elmo-msgdb-load temp-modb)
	      priorities (cdr priorities)))
      (when loaded
	(if (eq elmo-msgdb-convert-type 'auto)
	    (elmo-msgdb-append msgdb temp-modb)
	  (setq msgdb temp-modb))))
    msgdb))

(defun elmo-make-msgdb (&optional location type mime-charset)
  "Make a MSGDB."
  (let* ((type (or type elmo-msgdb-default-type))
	 (class (intern (format "modb-%s" type))))
    (require class)
    (luna-make-entity class
		      :location location
		      :mime-charset mime-charset)))

(defun elmo-msgdb-extra-fields (&optional non-virtual)
  (if non-virtual
      (apply
       #'nconc
       (mapcar
	(lambda (extra)
	  (let ((spec (assq (intern extra) modb-entity-field-extractor-alist)))
	    (if spec
		(let ((real-fields (nth 2 spec)))
		  (cond ((functionp real-fields)
			 (funcall real-fields extra))
			((listp real-fields)
			 (copy-sequence real-fields))))
	      (list extra))))
	elmo-msgdb-extra-fields))
    elmo-msgdb-extra-fields))

(defun elmo-msgdb-sort-by-date (msgdb)
  (elmo-msgdb-sort-entities
   msgdb
   (lambda (x y _app-data)
     (condition-case nil
	 (time-less-p
	  (elmo-message-entity-field x 'date)
	  (elmo-message-entity-field y 'date))
       (error)))))

(defsubst elmo-msgdb-get-parent-entity (entity msgdb)
  ;; Set parent-ids to entity.
  (setq entity (elmo-message-entity-field entity 'references))
  ;; In old msgdb, references's field is a string.
  (when (stringp entity)
    (setq entity (list entity)))
  (let (parent)
    (while entity
      (setq entity
	    (if (setq parent (elmo-msgdb-message-entity msgdb (car entity)))
		nil
	      (cdr entity))))
    parent))

;;;
(defsubst elmo-msgdb-append-element (list element)
  (if list
;;;      (append list (list element))
      (nconc list (list element))
    ;; list is nil
    (list element)))

;;
;; number <-> Message-ID handling
;;
(defsubst elmo-msgdb-number-add (alist number id)
  (let ((ret-val alist))
    (setq ret-val
	  (elmo-msgdb-append-element ret-val (cons number id)))
    ret-val))

;;; flag table
;;
(defvar elmo-flag-table-filename "flag-table")
(defun elmo-flag-table-load (dir)
  "Load flag hashtable for MSGDB."
  (let ((table (elmo-make-hash))
	;; For backward compatibility
	(seen-file (expand-file-name elmo-msgdb-seen-filename dir))
	value)
    (when (file-exists-p seen-file)
      (dolist (msgid (elmo-object-load seen-file))
	(elmo-set-hash-val msgid '(read) table))
      (delete-file seen-file))
    (dolist (pair (elmo-object-load
		   (expand-file-name elmo-flag-table-filename dir)))
      (setq value (cdr pair))
      (elmo-set-hash-val (car pair)
			 (cond ((consp value)
				value)
			       ;; Following cases for backward compatibility.
			       (value
				(list value))
			       (t
				'(unread)))
			 table))
    table))

(defun elmo-flag-table-set (flag-table msg-id flags)
  (elmo-set-hash-val msg-id (or flags '(read)) flag-table))

(defun elmo-flag-table-get (flag-table msg-id)
  (let ((flags (elmo-get-hash-val msg-id flag-table)))
    (append
     (and (elmo-file-cache-exists-p msg-id)
	  '(cached))
     (if flags
	 (elmo-list-delete '(cached read)
			   (copy-sequence flags)
			   #'delq)
       '(new unread)))))

(defun elmo-flag-table-save (dir flag-table)
  (elmo-object-save
   (expand-file-name elmo-flag-table-filename dir)
   (if flag-table
       (let (list)
	 (mapatoms (lambda (atom)
		     (setq list (cons (cons (symbol-name atom)
					    (symbol-value atom))
				      list)))
		   flag-table)
	 list))))
;;;
;; persistent mark handling
;; (for each folder)

(defun elmo-msgdb-mark-append (alist id mark)
  "Append mark."
  (setq alist (elmo-msgdb-append-element alist
					 (list id mark))))

(defun elmo-msgdb-flag-table (msgdb &optional flag-table)
  ;; Make a table of msgid flag (read, answered)
  (let ((flag-table (or flag-table
			(elmo-make-hash (elmo-msgdb-length msgdb))))
	msg-id)
    (dolist (number (elmo-msgdb-list-messages msgdb))
      (when (setq msg-id (elmo-msgdb-message-field msgdb number 'message-id))
	(elmo-flag-table-set flag-table
			     msg-id
			     (elmo-msgdb-flags msgdb number))))
    flag-table))

(defun elmo-multiple-fields-body-list (field-names &optional boundary)
  "Return list of each field-bodies of FIELD-NAMES of the message header
in current buffer. If BOUNDARY is not nil, it is used as message
header separator."
  (save-excursion
    (save-restriction
      (std11-narrow-to-header boundary)
      (let* ((case-fold-search t)
	     (s-rest field-names)
	     field-name field-body regexp)
	(while (setq field-name (car s-rest))
	  (setq regexp (concat "^" field-name ":[ \t]*"))
	  (goto-char (point-min))
	  (while (re-search-forward regexp nil t)
	    (setq field-body
		  (cons (buffer-substring-no-properties
			 (match-end 0) (std11-field-end))
			field-body)))
	  (setq s-rest (cdr s-rest)))
	(nreverse field-body)))))

(defsubst elmo-msgdb-remove-field-string (string)
  (if (string-match (concat std11-field-head-regexp "[ \t]*") string)
      (substring string (match-end 0))
    string))

(defsubst elmo-msgdb-seen-load (dir)
  (elmo-object-load (expand-file-name
		     elmo-msgdb-seen-filename
		     dir)))

(defsubst elmo-msgdb-out-of-date-messages (msgdb)
  (dolist (number (elmo-msgdb-list-flagged msgdb 'new))
    (elmo-msgdb-unset-flag msgdb number 'new)))

;;
;; deleted message handling
;;
(defun elmo-msgdb-killed-list-load (dir)
  (elmo-object-load
   (expand-file-name elmo-msgdb-killed-filename dir)
   nil t))

(defun elmo-msgdb-killed-list-save (dir killed-list)
  (elmo-object-save
   (expand-file-name elmo-msgdb-killed-filename dir)
   killed-list))

(defun elmo-msgdb-killed-message-p (killed-list msg)
  (elmo-number-set-member msg killed-list))

(defun elmo-msgdb-set-as-killed (killed-list msg)
  (elmo-number-set-append killed-list msg))

(defun elmo-msgdb-killed-list-length (killed-list)
  (let ((result (length killed-list)))
    (dolist (elt killed-list result)
      (when (consp elt)
	(setq result (+ result (cdr elt) (- (car elt))))))))

(defun elmo-msgdb-max-of-killed (killed-list)
  (let ((result 0))
    (dolist (elt killed-list result)
      (setq result (max result (if (consp elt) (cdr elt) elt))))))

(defun elmo-living-messages (messages killed-list)
  (if killed-list
      (delq nil
	    (mapcar (lambda (number)
		      (unless (elmo-number-set-member number killed-list)
			number))
		    messages))
    messages))

(defun elmo-msgdb-finfo-load ()
  (elmo-object-load (expand-file-name
		     elmo-msgdb-finfo-filename
		     elmo-msgdb-directory)
		    elmo-mime-charset t))

(defun elmo-msgdb-finfo-save (finfo)
  (elmo-object-save (expand-file-name
		     elmo-msgdb-finfo-filename
		     elmo-msgdb-directory)
		    finfo elmo-mime-charset))

(defun elmo-msgdb-flist-load (fname)
  (let ((flist-file (expand-file-name
		     elmo-msgdb-flist-filename
		     (expand-file-name
		      (elmo-safe-filename fname)
		      (expand-file-name "folder" elmo-msgdb-directory)))))
    (elmo-object-load flist-file elmo-mime-charset t)))

(defun elmo-msgdb-flist-save (fname flist)
  (let ((flist-file (expand-file-name
		     elmo-msgdb-flist-filename
		     (expand-file-name
		      (elmo-safe-filename fname)
		      (expand-file-name "folder" elmo-msgdb-directory)))))
    (elmo-object-save flist-file flist elmo-mime-charset)))

(defun elmo-crosspost-alist-load ()
  (elmo-object-load (expand-file-name
		     elmo-crosspost-alist-filename
		     elmo-msgdb-directory)
		    nil t))

(defun elmo-crosspost-alist-save (alist)
  (elmo-object-save (expand-file-name
		     elmo-crosspost-alist-filename
		     elmo-msgdb-directory)
		    alist))

(defsubst elmo-msgdb-location-load (dir)
  (let ((result (elmo-object-load
		 (expand-file-name
		  elmo-msgdb-location-filename
		  dir))))
    ;; The leading elements may be a list contains only max-number.
    (when result
      (while (null (cdar result))
	(setq result (cdr result))))
    result))

(defsubst elmo-msgdb-location-add (alist number location)
  (let ((ret-val alist))
    (setq ret-val
	  (elmo-msgdb-append-element ret-val (cons number location)))
    ret-val))

(defsubst elmo-msgdb-location-save (dir alist)
  (elmo-object-save
   (expand-file-name elmo-msgdb-location-filename dir)
   alist))

;;; For backward compatibility.
(defsubst elmo-msgdb-overview-entity-get-number (entity)
  (elmo-message-entity-number entity))

(defsubst elmo-msgdb-overview-entity-set-number (entity number)
  (elmo-message-entity-set-number entity number))

(defsubst elmo-msgdb-overview-entity-get-references (entity)
  (elmo-message-entity-field entity 'references))

(defsubst elmo-msgdb-overview-entity-set-references (entity references)
  (elmo-message-entity-set-field entity 'references references))

(defsubst elmo-msgdb-overview-entity-get-from-no-decode (entity)
  (mime-charset-encode-string
   (elmo-message-entity-field entity 'from) elmo-mime-charset))

(defsubst elmo-msgdb-overview-entity-get-from (entity)
  (elmo-message-entity-field entity 'from))

(defsubst elmo-msgdb-overview-entity-set-from (entity from)
  (elmo-message-entity-set-field entity 'from from))

(defsubst elmo-msgdb-overview-entity-get-subject (entity)
  (elmo-message-entity-field entity 'subject))

(defsubst elmo-msgdb-overview-entity-get-subject-no-decode (entity)
  (mime-charset-encode-string
   (elmo-message-entity-field entity 'subject) elmo-mime-charset))

(defsubst elmo-msgdb-overview-entity-set-subject (entity subject)
  (elmo-message-entity-set-field entity 'subject subject))

(defsubst elmo-msgdb-overview-entity-get-date (entity)
  (elmo-message-entity-field entity 'date 'string))

(defsubst elmo-msgdb-overview-entity-set-date (entity date)
  (elmo-message-entity-set-field entity 'date date))

(defsubst elmo-msgdb-overview-entity-get-to (entity)
  (elmo-message-entity-field entity 'to 'string))

(defsubst elmo-msgdb-overview-entity-get-cc (entity)
  (elmo-message-entity-field entity 'cc 'string))

(defsubst elmo-msgdb-overview-entity-get-size (entity)
  (elmo-message-entity-field entity 'size))

(defsubst elmo-msgdb-overview-entity-set-size (entity size)
  (elmo-message-entity-set-field entity 'size size))

(defsubst elmo-msgdb-overview-entity-get-extra (_entity)
  ;; Truely obsolete.
  )

(defsubst elmo-msgdb-overview-entity-set-extra (_entity _extra)
  ;; Truely obsolete.
  )

(defsubst elmo-msgdb-overview-entity-get-extra-field (entity
						      field-name)
  (elmo-message-entity-field entity (intern field-name)))

(defsubst elmo-msgdb-overview-entity-set-extra-field (entity
						      field-name
						      value)
  (elmo-message-entity-set-field entity (intern field-name) value))

(require 'product)
(product-provide (provide 'elmo-msgdb) (require 'elmo-version))

;;; elmo-msgdb.el ends here
