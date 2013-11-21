;;; jss-script.el -- major mode for viewing javascript files/snippets from the browser
;;
;; Copyright (C) 2013 Edward Marco Baringer
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE. See the GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

(eval-when-compile
  (require 'cl))
(require 'eieio)
(require 'jss-browser-api)
(require 'url)

(defvar jss-script-source-original-location-functions '()
  "A list of functions which, given a url, a line number and a
column number, return a file name, that emacs can find-file on,
which should be opened instead of a *JSS Script* buffer
containing the server's script contents.

Note: If jss finds a match it will open up the specified file but
will not check that the contents of the file match the code the
server has actually parsed and executed.")

(defun jss-script-prefix-match-source-location (prefix-url file-name-prefix)
  "Creates a function which well return a file-name that starts
with file-name-prefix if the passed in script url starts with
prefix-url.

If prefix-url specifies a schema (http or https) then it must
match the script's schema. the query args, fragment, user and
password values of prefix-url are ignored.

Note: prefix-url will be parse by `url-generic-parse-url`, if you
don't care about the schema use \"//example.com\", not simply
\"example.com\".

The value returned by this function is not intended to be used
directly, it should instead be put in the list
jss-script-source-original-location-functions."
  (lexical-let ((prefix-url prefix-url)
                (file-name-prefix file-name-prefix))
    (lambda (script-url line-number column-number)
      (message "Testing %s against %s." script-url prefix-url)
      (block nil
        (let ((prefix (url-generic-parse-url prefix-url))
              (script (url-generic-parse-url script-url)))
          (unless prefix (return nil))
          (unless script (return nil))
          (cl-flet ((url-part-match (part)
                                    (when (funcall part prefix)
                                      (unless (and (funcall part script)
                                                   (equal (funcall part script) (funcall part prefix)))
                                        (return nil)))))

            (url-part-match 'url-type)
            (url-part-match 'url-host)
            (url-part-match 'url-port)
            
            (when (string-prefix-p (car (url-path-and-query prefix))
                                   (car (url-path-and-query script)))
              (let ((file-name
                     (concat file-name-prefix (substring (car (url-path-and-query script))
                                                         (length (car (url-path-and-query prefix)))))))
                (message "File name: %s" file-name)
                (return file-name)))))))))

(make-variable-buffer-local
 (defvar jss-current-script nil))

(defun jss-script-mode* (script)
  (let ((jss-script script))
    (add-hook 'kill-buffer-hook 'jss-script-kill nil t)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (jss-script-body jss-script)))
    (if (fboundp 'js2-mode)
        (js2-mode)
      (js-mode))
    (setf buffer-read-only t
          jss-current-script jss-script)))

(defun jss-script-kill ()
  (interactive)
  (setf (jss-script-buffer jss-current-script) nil))

(defmethod jss-script-display-at-position ((script jss-generic-script) line-number column-number &optional force-server-side-js)
  (block found-buffer
    (when (and (jss-script-buffer script)
               (buffer-live-p (jss-script-buffer script)))
      (return-from found-buffer
       (jss-script-goto-offset script line-number column-number)))

    (unless force-server-side-js
      (loop
       for source-location-function in jss-script-source-original-location-functions
       for original-source = (funcall source-location-function
                                      (jss-script-url script)
                                      line-number
                                      column-number)
       when original-source
       do (setf (jss-script-buffer script) (find-file original-source))
       and do (return-from found-buffer
                (jss-script-goto-offset script line-number column-number))))

    (lexical-let ((script script)
                  (line-number line-number)
                  (column-number column-number))
      (return-from found-buffer
        (jss-deferred-then
         (jss-script-get-body script)
         (lambda (body)
           (setf (jss-script-buffer script) (generate-new-buffer (format "*JSS Script %s (%s)*" (jss-script-url script) (jss-script-id script)))
                 (jss-script-body script) body)
           (with-current-buffer (jss-script-buffer script)
             (jss-script-mode* script)
             (jss-script-goto-offset script line-number column-number))))))))

(defface jss-script-line-marker-face '((t :inherit highlight))
  "Face used to highlight the area around point."
  :group 'jss)

(defmethod jss-script-goto-offset ((script jss-generic-script) line-number column-number)
  "Ensure that the point ot line `line-number` and column
`column-number` of the script body of `script` is visible."
  (with-current-buffer (jss-script-buffer script)
    (let ((inhibit-read-only t))
      (goto-char (point-min))
      (dolist (o (overlays-in (point-min) (point-max)))
        (delete-overlay o))
      (forward-line line-number)
      (ignore-errors
        (forward-char column-number))
      (let* ((inhibit-read-only t)
             (end-of-line-point (save-excursion (end-of-line) (point)))
             (overlay (make-overlay (point) (min end-of-line-point (+ (point) 30))
                                    (current-buffer))))
        (overlay-put overlay 'face 'jss-script-line-marker-face)
        (display-buffer (current-buffer))
        (set-window-point (get-buffer-window (current-buffer)) (point))
        (with-selected-window (get-buffer-window (current-buffer))
          (recenter))))))

(provide 'jss-script)
