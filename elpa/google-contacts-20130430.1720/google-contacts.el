;;; google-contacts.el --- Support for Google Contacts in Emacs

;; Copyright © 2011 Julien Danjou <julien@danjou.info>

;; Author: Julien Danjou <julien@danjou.info>
;; Keywords: comm
;; Package-Requires: ((oauth2 "0.7"))

;; This file is NOT part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This allows you to access your Google Contacts from Emacs.
;;

;;; Code:

(require 'google-oauth)
(require 'url-cache)
(require 'widget)
(require 'xml)

(defgroup google-contacts nil
  "Google Contacts.")

(defconst google-contacts-oauth-client-id "209584303760.apps.googleusercontent.com"
  "Client ID for OAuth.")

(defconst google-contacts-oauth-client-secret "5SR3nk46hplfl-nfsSSaMxKc"
  "Google contacts secret key. Please don't tell anyone.
I AM SERIOUS!")

(defconst google-contacts-resource-url "https://www.google.com/m8/feeds"
  "URL used to request access to contacts resources.")

(defconst google-contacts-feed-url (concat google-contacts-resource-url
                                           "/contacts/default/full")
  "URL used to access contacts resources.")

(defcustom google-contacts-expire-time 86400
  "Time to keep contacts entry in cache without refreshing."
  :group 'google-contacts
  :type 'integer)

(defcustom google-contacts-max-result 1000
  "Result number limit to use when downloading contacts."
  :group 'google-contacts
  :type 'integer)

(defun google-contacts-oauth-token ()
  "Get OAuth token to access Google contacts."
  (google-oauth-auth-and-store
   google-contacts-resource-url
   google-contacts-oauth-client-id
   google-contacts-oauth-client-secret))

(defun google-contacts-url-retrieve (url &optional token)
  "Retrieve URL using cache if possible."
  (let ((url-cache-expire-time google-contacts-expire-time))
    (if (url-cache-expired url)
        (let ((buf (oauth2-url-retrieve-synchronously (or token
                                                          (google-contacts-oauth-token))
                                                      url)))
          ;; This is `url-store-in-cache' modified so it uses
          ;; `google-contacts-resource-url' to store the cache file as the
          ;; current URL, rathen than the URL with the access token.
          (with-current-buffer buf
            (let ((fname (url-cache-create-filename url)))
              (if (url-cache-prepare fname)
                  (let ((coding-system-for-write 'binary))
                    (write-region (point-min) (point-max) fname nil 5)))))
          buf)
      (url-fetch-from-cache url))))

(defun google-contacts-build-full-feed-url (&optional query-string)
  (concat google-contacts-feed-url
          "?v=3.0&max-results=" (number-to-string google-contacts-max-result)
          (when query-string (concat "&q=" (url-hexify-string query-string)))))

(defun google-contacts-fetch (&optional query-string token)
  "Fetch Google contacts data."
  (google-contacts-url-retrieve (google-contacts-build-full-feed-url query-string) token))

(defun google-contacts-http-data (buffer)
  "Return HTTP data from BUFFER."
  (with-current-buffer buffer
    (goto-char (point-min))
    (search-forward "\n\n" nil t)
    (let ((headers (buffer-substring (point-min) (point))))
      (unless (string-match-p "^HTTP/1.1 200 OK" headers)
        (kill-buffer)
        (error "Unable to fetch data"))
      (if (string-match-p "^Content-Type:.* charset=UTF-8" headers)
          (set-buffer-multibyte t)
        (set-buffer-multibyte nil)))
    (let ((data (buffer-substring (point) (point-max))))
      (kill-buffer)
      data)))

(defun google-contacts-data (&optional query-string token)
  "Get Google Contacts data using QUERY-STRING as query."
  ;; Just return the feed element, anyway that's the only one.
  (assoc 'feed
         (with-temp-buffer
           (insert (google-contacts-http-data
                    (google-contacts-fetch query-string token)))
           (xml-parse-region (point-min) (point-max)))))

(defun xml-node-get-attribute-type (element attribute)
  "Return the relation type of ELEMENT.
Usually work, home…"
  (let ((attr (xml-get-attribute element attribute)))
    (capitalize (or (nth 1 (split-string attr "#"))
                    attr))))

(defun xml-node-children-or-string (node)
  (let* ((children (xml-node-children node))
         (first-child (car children)))
    (if (stringp first-child)
        first-child
      children)))

(defun xml-node-child-string (node)
  (let ((child (xml-node-children-or-string node)))
    (if (stringp child)
        child
      "")))

(defmacro google-contacts-build-node-list (node node-name &optional value)
  "Build a list of values for each node matching NODE-NAME in NODE.
Return a list of value in format ((relation-type . value) … ).
If VALUE is not specified, we use the node value as a string."
  `(loop for child in (xml-get-children ,node ,node-name)
         collect (cons (xml-node-get-attribute-type child 'rel)
                       ,(or value '(xml-node-child-string child)))))

(defvar google-contacts-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'google-contacts-quit)
    (define-key map (kbd "m") 'google-contacts-mail)
    (define-key map (kbd "g") 'google-contacts-refresh)
    (define-key map (kbd "s") 'google-contacts)
    (define-key map (kbd "n") 'google-contacts-next)
    (define-key map (kbd "p") 'google-contacts-previous)
    map)
  "Keymap for `google-contacts-mode'.")

(set-keymap-parent google-contacts-mode-map widget-keymap)

(defun google-contacts-refresh ()
  "Refresh results."
  (interactive)
  (with-current-buffer (get-buffer-create google-contacts-buffer-name)
    (google-contacts google-contacts-query-string t)))

(defun google-contacts-next ()
  "Go to the next contact."
  (interactive)
  (goto-char (or (next-single-property-change
                  (+ (point) (if (get-text-property (point) 'google-contacts) 1 0))
                  'google-contacts)
                 (point))))

(defun google-contacts-previous ()
  "Go to the previous contact."
  (interactive)
  (goto-char (1- (or (previous-single-property-change
                      (if (get-text-property (point) 'google-contacts)
                          (point)
                        (or (1- (previous-single-property-change (point) 'google-contacts))
                            (point)))
                      'google-contacts)
                     (1+ (point))))))

(defun google-contacts-quit ()
  "Quit buffer."
  (interactive)
  (kill-buffer (current-buffer)))

(defun google-contacts-mail ()
  "Send a mail to the user at point."
  (interactive)
  (let ((mail-address (get-text-property (point) 'google-contacts-email)))
    (when mail-address
      (compose-mail mail-address))))

(defvar google-contacts-query-string nil
  "The query string used to make this query.")

(make-variable-buffer-local 'google-contacts-query-string)

(define-derived-mode google-contacts-mode fundamental-mode "Google Contacts"
  "A major mode for Google Contacts service"
  :group 'comm
  (setq buffer-read-only t))

(defvar google-contacts-history nil
  "`google-contacts' prompt history")

(defcustom google-contacts-margin-width 36
  "The margin width in pixels."
  :group 'google-contacts
  :type 'integer)

(defcustom google-contacts-separator-char ?\s
  "Char to used when drawing the separator."
  :group 'google-contacts
  :type 'character)

(defface google-contacts-givenname
  '((t (:height 1.4 :bold t)))
  "Face used to draw a separator between entries."
  :group 'google-contacts)

(defface google-contacts-familyname
  '((t (:height 1.4 :bold t)))
  "Face used to draw the family name."
  :group 'google-contacts)

(defface google-contacts-nickname
  '((t (:italic t)))
  "Face used to draw the nickname."
  :group 'google-contacts)

(defface google-contacts-organization-name
  '((t (:height 1.1 :italic t)))
  "Face used to draw the organization name."
  :group 'google-contacts)

(defface google-contacts-organization-title
  '((t (:height 1.1)))
  "Face used to draw the organization title."
  :group 'google-contacts)

(defface google-contacts-separator
  '((t (:background "grey20")))
  "Face used to draw a separator between entries."
  :group 'google-contacts)

(defface google-contacts-header
  '((t (:height 1.2 :bold t)))
  "Face used to draw the headers."
  :group 'google-contacts)

(defface google-contacts-rel
  '((t (:height 1.1 :bold t)))
  "Face used to draw the relation name."
  :group 'google-contacts)

(defface google-contacts-birthday nil
  "Face used to draw the relation name."
  :group 'google-contacts)

(defun google-contacts-margin-element ()
  "Return a string to insert to make a margin."
  (concat (propertize " " 'display `(space . (:width (,google-contacts-margin-width)))) " "))

(defun google-contacts-add-margin-to-text (text header-length)
  (replace-regexp-in-string "\n"
                            (concat "\n" (google-contacts-margin-element)
                                    (make-string header-length ? ))
                            text))

(defvar google-contacts-buffer-name "*Google Contacts*"
  "Buffer name for Google Contacts.")

(defun google-contacts-make-buffer ()
  "Prepare a buffer to output contacts information."
  (with-current-buffer (get-buffer-create google-contacts-buffer-name)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (google-contacts-mode)
      (current-buffer))))

(defun google-contacts-get-photo (query-string)
  "Retrieve one photo for QUERY-STRING.
This returns raw data as a string"
  (let ((token (google-contacts-oauth-token))
        (google-contacts-max-result 1))
    ;; Only get the first contact, so use `car'
    (let ((contact (car (xml-get-children (google-contacts-data query-string token) 'entry))))
      (when contact
        (let ((photo-url (loop for link in (xml-get-children contact 'link)
                               when (string= (xml-get-attribute link 'rel)
                                             "http://schemas.google.com/contacts/2008/rel#photo")
                               return (xml-get-attribute link 'href))))
          (google-contacts-http-data
           (google-contacts-url-retrieve photo-url token)))))))

;;;###autoload
(defun google-contacts (&optional query-string force-refresh)
  (interactive
   (list (read-string "Look for: " (car google-contacts-history)
                      'google-contacts-history)
         current-prefix-arg))
  (let ((buffer (google-contacts-make-buffer))
        (token (google-contacts-oauth-token))
        (google-contacts-expire-time (if force-refresh 0 google-contacts-expire-time))
        (inhibit-read-only t))
    ;; Switch to buffer right away so it is visible. We need it to be
    ;; visible to render things like the separator correctly.
    (unless (eq (current-buffer) buffer)
      (switch-to-buffer-other-window buffer))
    (setq google-contacts-query-string query-string)
    (let ((contacts (xml-get-children (google-contacts-data query-string token) 'entry)))
      (if contacts
          (progn
            (dolist (contact contacts)
              (let* ((name-value (nth 0 (xml-get-children contact 'gd:name)))
                     (fullname (xml-node-child-string (nth 0 (xml-get-children name-value 'gd:fullName))))
                     (givenname (xml-node-child-string (nth 0 (xml-get-children name-value 'gd:givenName))))
                     (familyname (xml-node-child-string (nth 0 (xml-get-children name-value 'gd:familyName))))

                     (nickname (xml-node-child-string (nth 0 (xml-get-children contact 'gContact:nickname))))
                     (birthday (xml-get-attribute-or-nil (nth 0 (xml-get-children contact 'gContact:birthday)) 'when))

                     (organization-value (nth 0 (xml-get-children contact 'gd:organization)))
                     (organization-name (xml-node-child-string (nth 0 (xml-get-children organization-value 'gd:orgName))))
                     (organization-title (xml-node-child-string (nth 0 (xml-get-children organization-value 'gd:orgTitle))))

                     (notes (xml-node-child-string (nth 0 (xml-get-children contact 'content))))
                     ;; Links
                     (links (xml-get-children contact 'link))
                     (photo-url (loop for link in links
                                      when (string= (xml-get-attribute link 'rel)
                                                    "http://schemas.google.com/contacts/2008/rel#photo")
                                      return (xml-get-attribute link 'href)))
                     ;; Multiple values
                     ;; Format is ((rel-type . data) (rel-type . data) … )
                     (events (google-contacts-build-node-list contact 'gContact:event
                                                              (xml-get-attribute (nth 0 (xml-get-children child 'gd:when)) 'startTime)))
                     (emails (google-contacts-build-node-list contact 'gd:email
                                                              (xml-get-attribute child 'address)))
                     (phones (google-contacts-build-node-list contact 'gd:phoneNumber))
                     (websites (google-contacts-build-node-list contact 'gContact:website
                                                                (xml-get-attribute child 'href)))
                     (relations (google-contacts-build-node-list contact 'gContact:relation))
                     (postal-addresses (google-contacts-build-node-list contact 'gd:structuredPostalAddress
                                                                        (xml-node-child-string
                                                                         (nth 0 (xml-get-children child 'gd:formattedAddress)))))
                     (instant-messaging (google-contacts-build-node-list contact 'gd:im
                                                                         (cons
                                                                          (xml-node-get-attribute-type child 'protocol)
                                                                          (cdr (assoc 'address (xml-node-attributes child))))))
                     (photo (ignore-errors
                              (create-image
                               (google-contacts-http-data
                                (google-contacts-url-retrieve photo-url token))
                               'imagemagick t :width google-contacts-margin-width :ascent 'center) ))
                     (beg (point)))
                (insert
                 (if photo
                     (concat (propertize " " 'display photo) " ")
                   (concat (propertize " " 'display `(space . (:width (,google-contacts-margin-width)
                                                                      :height (,google-contacts-margin-width)))) " "))
                 (propertize givenname 'face 'google-contacts-givenname) " "
                 (propertize familyname 'face 'google-contacts-familyname)
                 (if (string= nickname "")
                     ""
                   (concat " " (propertize (concat "(" nickname ")")  'face 'google-contacts-nickname))) "\n")

                (unless (and (string= organization-name "")
                             (string= organization-title ""))
                  (insert (google-contacts-margin-element) " "
                          (propertize organization-title 'face 'google-contacts-organization-title)
                          " @ "
                          (propertize organization-name 'face 'google-contacts-organization-name) "\n"))

                (google-contacts-insert-generic-list emails "E-mails"
                                                     (lambda (email)
                                                       (widget-create 'link
                                                                      :button-prefix "" :button-suffix ""
                                                                      :value (concat fullname " <" (cdr email) ">")
                                                                      :action (lambda (widget &optional _event)
                                                                                (compose-mail (widget-value widget)))
                                                                      :tag (cdr email))
                                                       ;; Return "" to insert nothing, since widget-create do the insertion.
                                                       ""))

                (google-contacts-insert-generic-list phones "Phones")
                (google-contacts-insert-generic-list postal-addresses "Addresses"
                                                     (lambda (address)
                                                       (google-contacts-add-margin-to-text (cdr address)
                                                                                           (+ 4 (length (car address))))))
                (google-contacts-insert-generic-list websites "Websites"
                                                     (lambda (website)
                                                       (widget-create 'url-link
                                                                      :button-prefix "" :button-suffix ""
                                                                      (cdr website))
                                                       ""))
                (google-contacts-insert-generic-list events "Events")
                (google-contacts-insert-generic-list relations "Relations"
                                                     (lambda (relation)
                                                       (widget-create 'link
                                                                      :button-prefix "" :button-suffix ""
                                                                      :action (lambda (widget &optional _event)
                                                                                (google-contacts (widget-value widget)))
                                                                      (cdr relation))
                                                       ""))
                (google-contacts-insert-generic-list instant-messaging "Instant messaging"
                                                     (lambda (im)
                                                       (concat (cddr im) " (" (cadr im) ")")))

                (when birthday
                  (insert "\n" (google-contacts-margin-element)
                          (propertize "Birthday:" 'face 'google-contacts-header)
                          " "
                          (propertize birthday 'face 'google-contacts-birthday)
                          "\n"))

                (unless (string= notes "")
                  (insert "\n" (google-contacts-margin-element)
                          (propertize "Notes:" 'face 'google-contacts-header)
                          " "
                          (propertize (google-contacts-add-margin-to-text notes 8)
                                      'face 'google-contacts-notes)
                          "\n"))

                ;; Insert properties
                (put-text-property beg (1+ beg) 'google-contacts t)
                (when emails
                  (put-text-property beg (point)
                                     'google-contacts-email (concat fullname " <" (cdr (nth 0 emails)) ">")))

                (insert "\n" (propertize (make-string (window-width) google-contacts-separator-char) 'face 'google-contacts-separator) "\n")))
            (goto-char (point-min)))
        ;; No contacts, insert a string and return nil
        (insert "No result."))
      ;; Return contacts
      contacts)))

(defun google-contacts-insert-generic-list (items title &optional get-value)
  "Insert a text for rendering ITEMS with TITLE.
Use GET-VALUE to get the value from the cdr of the item,
otherwise just put the cdr of item."
  (when items
    (insert "\n" (google-contacts-margin-element)
            (propertize (concat title ":\n") 'face 'google-contacts-header))
    (dolist (item items)
      (insert (google-contacts-margin-element) "  "
              (propertize (concat (car item) ":") 'face 'google-contacts-rel) " ")
      (insert (if get-value
                  (funcall get-value item)
                (cdr item))
              "\n"))))

(provide 'google-contacts)
