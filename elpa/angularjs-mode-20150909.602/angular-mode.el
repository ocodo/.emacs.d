;;; angular-mode.el --- Major mode for Angular.js

;; Angular Mode is based on JavaScript mode. It adds keyword
;; highlighting for important functions that are part of core
;; AngularJS.

;; Copyright (C) 2013 Rudolf Olah <omouse@gmail.com>

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;; Code:

(require 'cl)

(defvar angular-controller-definition-keywords
  '())

(defvar angular-directive-definition-keywords
  '("controller:"
    "controllerAs:"
    "link:"
    "scope:"
    "templateUrl:"
    "transclude:"))

(defvar angular-global-api-keywords
    '("angular.bind"
      "angular.bootstrap"
      "angular.copy"
      "angular.element"
      "angular.equals"
      "angular.extend"
      "angular.forEach"
      ".forEach"
      "angular.fromJson"
      "angular.identity"
      "angular.injector"
      "angular.isArray"
      "angular.isDate"
      "angular.isDefined"
      "angular.isElement"
      "angular.isFunction"
      "angular.isNumber"
      "angular.isObject"
      "angular.isString"
      "angular.isUndefined"
      "angular.lowercase"
      "angular.mock"
      "angular.module"
      ".module"
      "angular.noop"
      "angular.toJson"
      "angular.uppercase"
      "angular.version"
      ".directive"
      ".controller"
      ".service"
      ".factory"
      ;; rootScope.Scope keywords, https://docs.angularjs.org/api/ng/type/$rootScope.Scope
      "$new"
      "$watch"
      "$watchGroup"
      "$watchCollection"
      "$digest"
      "$destroy"
      "$eval"
      "$evalAsync"
      "$apply"
      "$on"
      "$emit"
      "$broadcast"
      "$id"
))

(defvar angular-services-keywords
  `("$anchorScroll"
    "$animate"
    "$cacheFactory"
    "$compile"
    "$controller"
    "$document"
    "$exceptionHandler"
    "$filter"
    "$http"
    "$httpBackend"
    "$interpolate"
    "$interval"
    "$locale"
    "$location"
    "$log"
    "$parse"
    "$q"
    "rootElement"
    "rootScope"
    "sce"
    "sceDelegate"
    "$templateCache"
    "$timeout"
    "$window"))

(defvar angular-mocha-keywords
  '(
    "describe("
    "beforeEach("
    "before("
    "afterEach("
    "it("
))

(defvar angular-font-lock-keywords
  `(
    (,(regexp-opt angular-global-api-keywords) . font-lock-builtin-face)
    (,(regexp-opt angular-services-keywords) . font-lock-builtin-face)
    (,(regexp-opt angular-controller-definition-keywords) . font-lock-type-face)
    (,(regexp-opt angular-directive-definition-keywords) . font-lock-type-face)
    (,(regexp-opt angular-mocha-keywords) . font-lock-type-face)
    ))

(define-derived-mode angular-mode javascript-mode
  "JavaScript[Angular]"
  "Major mode for AngularJS.
\\{javascript-mode-map}"
  (font-lock-add-keywords nil angular-font-lock-keywords))

(provide 'angular-mode)
;;; angular-mode.el ends here
