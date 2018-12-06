;; angular-html-mode.el --- Major mode for Angular.js HTML files

;; Copyright (C) 2013-2015 Rudolf Olah

;; Author: Rudolf Olah <omouse@gmail.com>

;; This file is not part of GNU Emacs.

;; URL: https://github.com/omouse/angularjs-mode
;; Keywords: languages javascript html

;;; Commentary:

;; Angular HTML Mode is based on HTML mode. It adds keyword
;; highlighting for directives that are part of core AngularJS.

;;; License:

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file. If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(defvar angular-html-font-lock-keywords
  (list
   (regexp-opt '("ng-app"
                 "ng-bind"
                 "ng-bind-html"
                 "ng-bind-template"
                 "ng-blur"
                 "ng-change"
                 "ng-checked"
                 "ng-class"
                 "ng-class-even"
                 "ng-class-odd"
                 "ng-click"
                 "ng-cloak"
                 "ng-controller"
                 "ng-copy"
                 "ng-csp"
                 "ng-cut"
                 "ng-dblclick"
                 "ng-disabled"
                 "ng-focus"
                 "ng-form"
                 "ng-hide"
                 "ng-href"
                 "ng-if"
                 "ng-include"
                 "ng-init"
                 "ng-keydown"
                 "ng-keypress"
                 "ng-keyup"
                 "ng-list"
                 "ng-model"
                 "ng-mousedown"
                 "ng-mouseenter"
                 "ng-mouseleave"
                 "ng-mousemove"
                 "ng-mouseover"
                 "ng-mouseup"
                 "ng-non-bindable"
                 "ng-open"
                 "ng-paste"
                 "ng-pluralize"
                 "ng-readonly"
                 "ng-repeat"
                 "ng-selected"
                 "ng-show"
                 "ng-src"
                 "ng-srcset"
                 "ng-style"
                 "ng-submit"
                 "ng-switch"
                 "ng-transclude"
                 "ng-value"))
   "{{.+?}}" ; angular expressions
))

;;;###autoload
(define-derived-mode angular-html-mode
  html-mode
  "HTML[Angular]"
  "Major HTML mode for AngularJS.
\\{html-mode-map}"

  (defvar sgml-font-lock-keywords)
  (defvar sgml-font-lock-keywords-1)
  (defvar sgml-font-lock-keywords-2)

  (setq font-lock-defaults (list
                            (append sgml-font-lock-keywords
                                    sgml-font-lock-keywords-1
                                    sgml-font-lock-keywords-2
                                    angular-html-font-lock-keywords))))

(provide 'angular-html-mode)
;;; angular-html-mode.el ends here
