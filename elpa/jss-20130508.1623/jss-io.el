;;; jss-io.el -- major mode for viewing browser request/response pairs
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
(require 'jss-io-pretty-printers)
(require 'jss-http-repl)

(defvar jss-io nil
  "Dummy variable used to pass data to the function jss-io-mode.")

(make-variable-buffer-local
 (defvar jss-current-io-object))

(define-derived-mode jss-io-mode jss-super-mode "JSS IO"
  "Major mode for viewing network transactions (HTTP
request-response pairs) between the browser and the server(s).

The mode is fairly simple, it serves only to present information
about the request and the response in a useful way. Each set of
headers is in its own section, by default invisibile but RET on
the section label will show it.

The request data is also in its own section and is shown exactly
as it was sent to the server.

If jss has a pretty-printer for the response's content-type (as
we do for html, css, js and json data) it will be preprocessed
and shown in a, hopefully, more convenient form. Use the [view
raw] button to show the actual bytes sent back from the server."
  (setf jss-current-io-object jss-io
        (jss-io-buffer jss-io) (current-buffer))

  (insert "Request: ")
  (jss-insert-with-highlighted-whitespace (jss-io-request-method jss-io))
  (insert " ")
  (jss-insert-with-highlighted-whitespace (jss-io-request-url jss-io))
  (insert "\n")

  (insert "Response: ")
  (if (jss-io-response-status jss-io)
      (insert (jss-io-response-status jss-io))
    (insert "None received."))
  (insert "\n")

  (jss-toggling-visibility
   (lambda ()
     (insert "Request Headers:"))
   (lambda ()
     (insert "\n")
     (jss-wrap-with-text-properties `(jss-request-headers t)
       (jss-toggling-sections
        "[view raw]\n"
        (lambda ()
          (jss-wrap-with-text-properties (list 'jss-response-headers t)
            (jss-io-insert-header-table (jss-io-request-headers jss-io) :indent 2)))
        "[view parsed]\n"
        (lambda ()
          (insert (jss-io-raw-request-headers jss-io))
          (insert "\n"))))))

  (when (jss-io-request-data jss-io)
    (jss-toggling-visibility
     (lambda ()
       (insert "Request Data:"))
     (lambda ()
       (insert "\n")
       (jss-wrap-with-text-properties `(jss-request-data t)
         (insert (jss-io-request-data jss-io)))
       (insert "\n"))
     :initially-visibile (< (length (jss-io-request-data jss-io)) (window-width (get-buffer-window (current-buffer))))))
  
  (when (jss-io-response-status jss-io)
    (jss-toggling-visibility
     (lambda ()
       (insert "Response Headers:"))
     (lambda ()
       (insert "\n")
       (jss-toggling-sections
        "[view raw]\n"
        (lambda ()
          (jss-wrap-with-text-properties (list 'jss-response-headers t)
            (jss-io-insert-header-table (jss-io-response-headers jss-io) :indent 2)))
        "[view parsed]\n"
        (lambda ()
          (insert (jss-io-raw-response-headers jss-io))
          (insert "\n")))))

    (jss-toggling-visibility
     (lambda ()
       (insert "Response Data: ")
       (when (jss-io-response-content-type jss-io)
         (insert "type: " (jss-io-response-content-type jss-io)))
       (when (jss-io-response-content-length jss-io)
         (insert "length: " (jss-io-response-content-length jss-io))))
     (lambda ()
       (insert "\n")
       (let ((data (jss-io-response-data jss-io)))
         (if data
             (jss-io-insert-response-data jss-io)
           (insert "no data."))))
     :initially-visibile t))

  (read-only-mode 1)
  (goto-char (point-min)))

(defun jss-current-io () jss-current-io-object)

(easy-menu-define jss-io-mode-menu jss-io-mode-map "JSS IO Menu"
  '("JSS IO"
    [ "Close" 'kill-buffer t]
    [ "Edit as new request" jss-io-clone-into-http-repl t ]))

(define-key jss-io-mode-map (kbd "q") (lambda () (interactive) (kill-buffer (current-buffer))))

(defun* jss-io-insert-header-table (header-alist &key indent)
  (when header-alist
    (let* ((headers (mapcar (lambda (h)
                              (if (stringp (car h))
                                  (cons (car h) (cdr h))
                                (cons (format "%s" (car h)) (cdr h))))
                            header-alist))
           (longest-key (loop
                         for header in headers
                         for max = (max (or max 0) (length (car header)))
                         finally (return (+ 4 max)))))
      (dolist (header headers)
        (let ((start (point)))
          (when indent
            (insert (make-string indent ?\s)))
          (jss-insert-with-highlighted-whitespace (car header))
          (insert ": ")
          (insert (make-string (- longest-key (- (point) start)) ?\s))
          (insert (cdr header) "\n"))))))

(defun jss-console-switch-to-io-inspector (io)
  (interactive (list (jss-tab-get-io (jss-current-tab) (get-text-property (point) 'jss-io-id))))
  (unless io
    (error "io is nil. not good."))

  (if (and (jss-io-buffer io)
           (buffer-live-p (jss-io-buffer io)))
      (display-buffer (jss-io-buffer io))
    (setf (jss-io-buffer io)
          (get-buffer-create (generate-new-buffer-name (format "*JSS IO %s*" (jss-io-id io)))))
    (with-current-buffer (jss-io-buffer io)
      (let ((jss-io io))
        (jss-io-mode)))
    (display-buffer (jss-io-buffer io))))

(defmethod jss-io-insert-response-data ((io jss-generic-io))
  (let* ((content-type (jss-io-response-content-type io))
         (cleaner (gethash content-type jss-io-cleaners))
         (response-data (jss-io-response-data io))
         (string (ignore-errors
                   (if cleaner
                       (funcall cleaner response-data)
                     nil))))
    (if string
        (jss-toggling-sections
         "[view raw]"
         (lambda ()
           (insert "\n" string))
         "[view parsed]"
         (lambda ()
           (insert response-data)))
      (insert response-data))))

(defun jss-io-clone-into-http-repl ()
  (interactive)
  (let ((jss-io (jss-current-io)))
    (unless (jss-current-io)
      (error "No IO object in this buffer."))
    (switch-to-buffer-other-window
     (jss-http-repl-new :header-string  (jss-io-raw-request-headers jss-io)
                        :data-string  (jss-io-request-data jss-io)
                        :url (jss-io-request-url jss-io)
                        :method (jss-io-request-method jss-io)))))

(provide 'jss-io)
