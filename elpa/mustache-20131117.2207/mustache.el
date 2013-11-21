;;; mustache.el --- a mustache templating library in emacs lisp

;; Copyright (C) 2013 Wilfred Hughes

;; Author: Wilfred Hughes <me@wilfred.me.uk>
;; Version: 0.23
;; Keywords: mustache, template
;; Package-Requires: ((ht "0.9") (s "1.3.0") (dash "1.2.0"))

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

;; See documentation at https://github.com/Wilfred/mustache.el

;; Note on terminology: We treat mustache templates as a sequence of
;; strings (plain text), and tags (anything wrapped in delimeters:
;; {{foo}}). A section is a special tag that requires closing
;; (e.g. {{#foo}}{{/foo}}).

;; We treat mustache templates as if they conform to a rough grammar:

;; TEMPLATE = plaintext | TAG | SECTION | TEMPLATE TEMPLATE
;; SECTION = OPEN-TAG TEMPLATE CLOSE-TAG
;; TAG = "{{" text "}}"

;; Public functions are of the form `mustache-FOO`, private
;; functions/variables are of the form `mst--FOO`.

;;; Code:

(load "mustache-render.el")

;; todo: add flag to set tolerance of missing variables
(defun mustache-render (template context)
  "Render a mustache TEMPLATE with hash table CONTEXT."
  (mst--render template context))

(defvar mustache-partial-paths nil
  "A list of paths to be searched for mustache partial templates (files ending .mustache).")

(defvar mustache-key-type 'string
  "What type of key we expect in contexts.
Can take the value 'string or 'keyword.

For 'string we expect contexts of the form:
#s\(hash-table data \(\"name\" \"J. Random user\"\)\)

for 'keyword we expect contexts of the form:
#s\(hash-table data \(:name \"J. Random user\"\)\)
")

(provide 'mustache)
;;; mustache.el ends here
