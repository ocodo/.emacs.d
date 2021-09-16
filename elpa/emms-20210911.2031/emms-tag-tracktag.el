;;; emms-tag-tracktag.el --- EMMS interface for audiotools tracktag  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Free Software Foundation, Inc.

;; Author: Grant Shoshin Shangreaux <grant@churls.world>
;; Keywords:

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

;; Provides a wrapper for audiotools tracktag executable
;; http://audiotools.sourceforge.net/tracktag.html
;; Given an EMMS TRACK structure, it will map the emms-info fields onto
;; arguments for tracktag. Then it calls the tracktag process to write the
;; info as metadata tags on the track's associated file.

;;; Code:

(require 'emms)

(defvar emms-tag-tracktag--info-fields
  '((info-artist . artist)
    (info-composer . composer)
    (info-performer . performer)
    (info-title . name)
    (info-album . album)
    (info-tracknumber . number)
    (info-discnumber . album-number)
    (info-year . year)
    (info-date . date)
    (info-note . comment))
  "An alist mapping info-* fields to tracktag fields.")

(defvar emms-tag-tracktag-log-buffer "*EMMS-LOG*"
  "Name of emms-tag-tracktag's log buffer.
Defaults to the same value as emms-tag-editor-log-buffer")

(defun emms-tag-tracktag--map-track-info (track)
  (seq-filter (lambda (cell) (cdr cell))
              (mapcar (lambda (pair)
                        (cons (cdr pair) (emms-track-get track (car pair))))
                      emms-tag-tracktag--info-fields)))

(defun emms-tag-tracktag--build-args (track)
  (flatten-list
   (append
    (mapcar (lambda (pair)
              (let ((tag (car pair)) (value (cdr pair)))
                (when value
                  ;; if we've deleted a tag value in the editor, remove the tag from file metadata.
                  (if (string-equal "" value) (concat "--remove-" (format "%s" tag))
                    (concat "--" (format "%s" tag) "=" value)))))
            (emms-tag-tracktag--map-track-info track))
    (list (emms-track-name track)))))

(defun emms-tag-tracktag-file (track)
  (apply #'call-process
   "tracktag" nil
   (get-buffer-create emms-tag-tracktag-log-buffer)
   nil
   "-Vdebug"
   (emms-tag-tracktag--build-args track)))

(provide 'emms-tag-tracktag)
;;; emms-tag-tracktag.el ends here
