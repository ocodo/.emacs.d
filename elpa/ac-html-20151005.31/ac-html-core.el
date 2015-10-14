;;; ac-html-core.el --- auto complete html core      -*- lexical-binding: t; -*-

;; Copyright (C) 2015 Zhang Kai Yu

;; Author: Zhang Kai Yu <yeannylam@gmail.com>

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

;;

;;; Code:

(require 'auto-complete)
(require 'cl-lib)
(require 'dash)

;;; Customization

(defgroup auto-complete-html nil
  "HTML Auto Complete."
  :group 'auto-complete
  :prefix "ac-html-")

;;; Variables

(defvar ac-html-data-providers nil "Completion data providers.")

(defvar-local ac-html-enabled-data-providers nil
  "The enabled data providers of current buffer.")

(defvar-local ac-html-current-tag-function nil
  "The function to find current tag.")

(defvar-local ac-html-current-attr-function nil
  "The function to find current attr.")

;;; Provider

;;;###autoload
(defmacro ac-html-define-data-provider (provider &rest pairs)
  "Define ac-html data provider with this macro.
This macro is buggy and cannot be used now."
  (declare (indent 1) (debug t))
  (let (tag-func attr-func attrv-func id-func class-func tag-doc-func
                 attr-doc-func attrv-doc-func id-doc-func class-doc-func)
    (let (label value)
      (while (not (= 0 (length pairs)))
        (setq label (pop pairs))
        (setq value (pop pairs))
        (and (equal :tag-func label) (setq tag-func value))
        (and (equal :attr-func label) (setq attr-func value))
        (and (equal :attrv-func label) (setq attrv-func value))
        (and (equal :id-func label) (setq id-func value))
        (and (equal :class-func label) (setq class-func value))
        (and (equal :tag-doc-func label) (setq tag-doc-func value))
        (and (equal :attr-doc-func label) (setq attr-doc-func value))
        (and (equal :attrv-doc-func label) (setq attrv-doc-func value))
        (and (equal :id-doc-func label) (setq id-doc-func value))
        (and (equal :class-doc-func label) (setq class-doc-func value))))
    `(progn
       (add-to-list 'ac-html-data-providers ,provider)
       (put ,provider :tag-func ,tag-func)
       (put ,provider :attr-func ,attr-func)
       (put ,provider :attrv-func ,attrv-func)
       (put ,provider :id-func ,id-func)
       (put ,provider :class-func ,class-func)
       (put ,provider :tag-doc-func ,tag-doc-func)
       (put ,provider :attr-doc-func ,attr-doc-func)
       (put ,provider :attrv-doc-func ,attrv-doc-func)
       (put ,provider :id-doc-func ,id-doc-func)
       (put ,provider :class-doc-func ,class-doc-func))))

;;;###autoload
(defun ac-html-enable-data-provider (provider)
  "Enable data provider PROVIDER."
  (add-to-list 'ac-html-enabled-data-providers provider))

(defun ac-html-query-data-provider (provider key)
  (get provider key))

;;; Language (Adaptor)

;;;###autoload
(defmacro ac-html-define-ac-source (lang &rest pairs)
  "Define ac-html lang with this macro."
  (declare (indent 1) (debug t))
  (let (tag-prefix attr-prefix attrv-prefix id-prefix class-prefix
                   current-tag-func current-attr-func)
    (let (label value)
      (while (not (= 0 (length pairs)))
        (setq label (pop pairs))
        (setq value (pop pairs))
        (and (equal :tag-prefix label) (setq tag-prefix value))
        (and (equal :attr-prefix label) (setq attr-prefix value))
        (and (equal :attrv-prefix label) (setq attrv-prefix value))
        (and (equal :id-prefix label) (setq id-prefix value))
        (and (equal :class-prefix label) (setq class-prefix value))
        (and (equal :current-tag-func label) (setq current-tag-func value))
        (and (equal :current-attr-func label) (setq current-attr-func value))))

    `(progn
       (defun ,(intern (format "ac-%s-setup" lang)) ()
         ,(format "Setup for ac-html to provide completion for %s language." lang)
         (setq ac-html-current-tag-function (quote ,current-tag-func))
         (setq ac-html-current-attr-function (quote ,current-attr-func)))
       ,(if tag-prefix
            `(ac-define-source ,(format "%s-%s" lang "tag")
               '((candidates . ac-html-all-tag-candidates)
                 (prefix . ,tag-prefix)
                 (document . ac-html-tag-documentation)
                 (symbol . "t"))))
       ,(if attr-prefix
            `(ac-define-source ,(format "%s-%s" lang "attr")
               '((candidates . ac-html-all-attr-candidates)
                 (prefix . ,attr-prefix)
                 (document . ac-html-attr-documentation)
                 (symbol . "a"))))
       ,(if attrv-prefix
            `(ac-define-source ,(format "%s-%s" lang "attrv")
               '((candidates . ac-html-all-attrv-candidates)
                 (prefix . ,attrv-prefix)
                 (document . ac-html-attrv-documentation)
                 (symbol . "v"))))
       ,(if id-prefix
            `(ac-define-source ,(format "%s-%s" lang "id")
               '((candidates . ac-html-all-id-candidates)
                 (prefix . ,id-prefix)
                 (document . ac-html-id-documentation)
                 (symbol . "i"))))
       ,(if class-prefix
            `(ac-define-source ,(format "%s-%s" lang "class")
               '((candidates . ac-html-all-class-candidates)
                 (prefix . ,class-prefix)
                 (document . ac-html-class-documentation)
                 (symbol . "c"))))
       )))

;;; Data

(defun ac-html-all-tag-candidates ()
  "All tag candidates get from data providers."
  (let (list func)
    (dolist (provider ac-html-enabled-data-providers)
      (setq func (ac-html-query-data-provider provider :tag-func))
      (if func (setq list (-concat list (funcall func)))))
    list))

(defun ac-html-all-attr-candidates ()
  "All attr candidates get from data providers."
  (let (list func tag)
    (dolist (provider ac-html-enabled-data-providers)
      (setq func (ac-html-query-data-provider provider :attr-func))
      (setq tag (funcall ac-html-current-tag-function))
      (if func (setq list (-concat list (funcall func tag)))))
    list))

(defun ac-html-all-attrv-candidates ()
  "All attrv candidates get from data providers."
  (let (list func tag attr)
    (dolist (provider ac-html-enabled-data-providers)
      (setq func (ac-html-query-data-provider provider :attrv-func))
      (setq tag (funcall ac-html-current-tag-function))
      (setq attr (funcall ac-html-current-attr-function))
      (if func (setq list (-concat list (funcall func tag attr)))))
    (if (string= attr "class")
        (setq list (-concat list (ac-html-all-class-candidates))))
    (if (string= attr "id")
        (setq list (-concat list (ac-html-all-id-candidates))))
    list))

(defun ac-html-all-id-candidates ()
  ""
  (let (list func)
    (dolist (provider ac-html-enabled-data-providers)
      (setq func (ac-html-query-data-provider provider :id-func))
      (if func (setq list (-concat list (funcall func)))))
    list))

(defun ac-html-all-class-candidates ()
  ""
  (let (list func)
    (dolist (provider ac-html-enabled-data-providers)
      (setq func (ac-html-query-data-provider provider :class-func))
      (if func (setq list (-concat list (funcall func)))))
    list))

(defun ac-html-tag-documentation (tag)
  "Not documented yet."
  (catch 'return-val
    (let (doc func)
      (dolist (provider ac-html-enabled-data-providers)
        (setq func (ac-html-query-data-provider provider :tag-doc-func))
        (if func
            (progn
              (setq doc (funcall func tag))
              (if doc (throw 'return-val doc))))))))

(defun ac-html-attr-documentation (attr)
  "Not documented yet."
  (catch 'return-val
    (let (doc func tag)
      (dolist (provider ac-html-enabled-data-providers)
        (setq func (ac-html-query-data-provider provider :attr-doc-func))
        (if func
            (progn
              (setq tag (funcall ac-html-current-tag-function))
              (setq doc (funcall func tag attr))
              (if doc (throw 'return-val doc))))))))

(defun ac-html-attrv-documentation (attrv)
  "Not documented yet."
  (catch 'return-val
    (let (doc func tag attr)
      (dolist (provider ac-html-enabled-data-providers)
        (setq func (ac-html-query-data-provider provider :attrv-doc-func))
        (if func
            (progn
              (setq tag (funcall ac-html-current-tag-function))
              (setq attr (funcall ac-html-current-attr-function))
              (setq doc (funcall func tag attr attrv))
              (if doc (throw 'return-val doc)))))
      (if (string= attr "class")
          (throw 'return-val (ac-html-class-documentation attrv)))
      (if (string= attr "id")
          (throw 'return-val (ac-html-id-documentation attrv))))))

(defun ac-html-id-documentation (id)
  ""
  (catch 'return-val
    (let (doc func)
      (dolist (provider ac-html-enabled-data-providers)
        (setq func (ac-html-query-data-provider provider :id-doc-func))
        (if func
            (progn
              (setq doc (funcall func id))
              (if doc (throw 'return-val doc))))))))

(defun ac-html-class-documentation (class)
  ""
  (catch 'return-val
    (let (doc func)
      (dolist (provider ac-html-enabled-data-providers)
        (setq func (ac-html-query-data-provider provider :class-doc-func))
        (if func
            (progn
              (setq doc (funcall func class))
              (if doc (throw 'return-val doc))))))))

(provide 'ac-html-core)
;;; ac-html-core.el ends here
