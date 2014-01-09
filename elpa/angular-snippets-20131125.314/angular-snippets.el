;;; angular-snippets.el --- Yasnippets for AngularJS

;; Copyright (C) 2013 Magnar Sveen

;; Author: Magnar Sveen <magnars@gmail.com>
;; Keywords: snippets
;; Version: 0.2.3
;; Package-Requires: ((s "1.4.0") (dash "1.2.0"))

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

;; Yasnippets for [AngularJS](http://angularjs.org/).

;;; Code:

(require 'yasnippet)
(require 'dash)
(require 's)

;;;###autoload
(defun ng-snip-show-docs-at-point ()
  (interactive)
  (ng-snip/show-or-browse-docs (ng-snip/closest-ng-identifer)))

(defvar ng-directive-docstrings
  '(("ng-app" . "Auto-bootstraps an application, with optional module to load.")
    ("ng-bind" . "Replace text content of element with value of given expression.")
    ("ng-bind-html-unsafe" . "Set innerHTML of element to unsanitized value of given expression.")
    ("ng-bind-template" . "Replace text content of element with given template.")
    ("ng-change" . "Eval the given expression when user changes the input. Requires ng-model.")
    ("ng-checked" . "Uses given expression to determine checked-state of checkbox.")
    ("ng-class" . "Sets class names on element based on given expression.")
    ("ng-class-even" . "Like ng-class, but only on even rows. Requires ng-repeat.")
    ("ng-class-odd" . "Like ng-class, but only on odd rows. Requires ng-repeat.")
    ("ng-click" . "Eval the given expression when element is clicked.")
    ("ng-cloak" . "Hides the element contents until compiled by angular.")
    ("ng-controller" . "Assign controller to this element, along with a new scope.")
    ("ng-csp" . "Enables Content Security Policy support. Should be on same element as ng-app.")
    ("ng-dblclick" . "Eval the given expression when element is double clicked.")
    ("ng-disabled" . "Uses given expression to determine disabled-state of element.")
    ("ng-form" . "Nestable alias of the form directive.")
    ("ng-hide" . "Hides the element if the expression is truthy.")
    ("ng-href" . "Avoids bad URLs on links that are clicked before angular compiles them.")
    ("ng-include" . "Fetches, compiles and includes an external HTML fragment.")
    ("ng-init" . "Evals expression before executing template during bootstrap.")
    ("ng-list" . "Text input that converts between comma-separated string and an array of strings.")
    ("ng-model" . "Sets up two-way data binding. Works with input, select and textarea.")
    ("ng-mousedown" . "Eval the given expression on mousedown.")
    ("ng-mouseenter" . "Eval the given expression on mouseenter.")
    ("ng-mouseleave" . "Eval the given expression on mouseleave.")
    ("ng-mousemove" . "Eval the given expression on mousemove.")
    ("ng-mouseover" . "Eval the given expression on mouseover.")
    ("ng-mouseup" . "Eval the given expression on mouseup.")
    ("ng-multiple" . "Uses given expression to determine multiple-state of select element.")
    ("ng-non-bindable" . "Makes angular ignore {{bindings}} inside element.")
    ("ng-options" . "Populates select options from a list or object.")
    ("ng-pluralize" . "Helps change wording based on a number.")
    ("ng-readonly" . "Uses given expression to determine readonly-state of element.")
    ("ng-repeat" . "Repeats template for every item in a list.")
    ("ng-selected" . "Uses given expression to determine selected-state of option element.")
    ("ng-show" . "Hides the element if the expression is falsy.")
    ("ng-src" . "Stops browser from fetching images with {{templates}} in the URL.")
    ("ng-style" . "Sets style attributes from an object of DOM style properties. ")
    ("ng-submit" . "Eval the given expression when form is submitted, and prevent default.")
    ("ng-switch" . "Switch on given expression to conditionally change DOM structure.")
    ("ng-switch-when" . "Include this element if value matches ng-switch on expression.")
    ("ng-transclude" . "Signifies where to insert transcluded DOM.")
    ("ng-view" . "Signifies where route views are shown.")))

(defvar ng-snip/docs-indirection
  '(("ng-options" . "select")
    ("ng-switch-when" . "ng-switch")))

(defvar ng-snip/directive-root-url
  "http://docs.angularjs.org/api/ng.directive:")

(defun -aget (alist key)
  (cdr (assoc key alist)))

(defun ng-snip/directive-to-docs (directive)
  (let ((name (car directive))
        (docstring (cdr directive)))
    (list name
          :docstring docstring
          :docurl (s-with (or (-aget ng-snip/docs-indirection name) name)
                    (s-lower-camel-case)
                    (concat ng-snip/directive-root-url)))))

(setq ng-docs (-map 'ng-snip/directive-to-docs ng-directive-docstrings))

(defun ng-snip/docs-value (id prop)
  (plist-get (-aget ng-docs id) prop))

(defvar ng-snip/last-docs-message nil)

(defun ng-snip/forget-last-docs-message ()
  (setq ng-snip/last-docs-message nil))

(defun ng-snip/docs (id)
  (message (ng-snip/docs-value id :docstring))
  (setq ng-snip/last-docs-message id)
  (run-with-timer 10.0 nil 'ng-snip/forget-last-docs-message)
  nil)

(defun ng-snip/show-or-browse-docs (id)
  (if (s-equals? ng-snip/last-docs-message id)
      (ng-snip/browse-docs id)
    (ng-snip/docs id)))

(defun ng-snip/browse-docs (id)
  (browse-url (ng-snip/docs-value id :docurl)))

(defun ng-snip/maybe-space-after-attr ()
  (unless (looking-at "[ />]\\|$")
    (insert " ")))

(defun ng-snip/closest-ng-identifer ()
  (save-excursion
    (forward-char 3)
    (search-backward "ng-")
    (unless (looking-at "ng-[a-z\-]+")
      (error "No angular identifier at point"))
    (match-string 0)))

(setq angular-snippets-root (file-name-directory (or load-file-name
                                                    (buffer-file-name))))

;;;###autoload
(defun angular-snippets-initialize ()
  (let ((snip-dir (expand-file-name "snippets" angular-snippets-root)))
    (when (boundp 'yas-snippet-dirs)
      (add-to-list 'yas-snippet-dirs snip-dir t))
    (yas/load-directory snip-dir)))

;;;###autoload
(eval-after-load "yasnippet"
  '(angular-snippets-initialize))

(provide 'angular-snippets)
;;; angular-snippets.el ends here
