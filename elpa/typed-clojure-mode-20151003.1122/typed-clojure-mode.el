;;; typed-clojure-mode.el --- Typed Clojure minor mode for Emacs

;; Copyright © 2014 John Walker
;;
;; Author: John Walker <john.lou.walker@gmail.com>, Ambrose Bonnaire-Sergeant <abonnairesergeant@gmail.com>
;; URL: https://github.com/typedclojure/typed-clojure-mode
;; Version: 1.1.0
;; Package-Requires: ((clojure-mode "2.1.1") (cider "0.10.0-cvs"))

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

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Provides helpers for getting things done in Typed Clojure

;;; Code:

(require 'button)
(require 'cider)
(require 'cider-client)
(require 'clojure-mode)
(require 'typed-clojure-error-mode)

(defvar typed-clojure-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-x n") 'typed-clojure-check-ns)
    (define-key map (kbd "C-c C-x f") 'typed-clojure-check-form)
    (define-key map (kbd "C-c C-a v") 'typed-clojure-ann-var)
    (define-key map (kbd "C-c C-a f") 'typed-clojure-ann-form)
    map))

;;;###autoload
(define-minor-mode typed-clojure-mode
  "The official minor mode for editing Typed Clojure. Provides
namespace typechecking, error navigation, display of type data,
and annotation snippets.

\\{typed-clojure-mode-map}"
  :group 'typed-clojure
  :lighter " Typed"
  :keymap typed-clojure-mode-map)

(defconst typed-clojure-current-alias-clj
  "(if-let [[al typedns] (first (filter #(=
                                       (find-ns 'clojure.core.typed)
                                       (val %))
                                     (ns-aliases *ns*)))]
  (str al \"/\")
  \"clojure.core.typed/\")")

(defconst typed-clojure-clj-qualify-ann-var
  "(let [s '%s
        ^clojure.lang.Var v (when (symbol? s) (resolve s))]
    (cond
     ; if unresolved just insert whatever is given
     (not (var? v))
       (when (symbol? s) (str s))
     ; fully qualify all vars outside current namespace
     ; also add :no-check prefix
     (not= *ns* (.ns v))
       (str \"^:no-check \"
            (symbol (str (ns-name (.ns v)))
                    (str (.sym v))))
     :else
       (str (name (symbol s)))))"
  )

(defun typed-clojure-read-eval-sync (s)
  (read (nrepl-dict-get (cider-nrepl-sync-request:eval s)
                        "value")))

(defun typed-clojure-qualify-ann-var (n)
  (typed-clojure-read-eval-sync
   (format typed-clojure-clj-qualify-ann-var n)))

(defun typed-clojure-current-alias ()
  (typed-clojure-read-eval-sync
   typed-clojure-current-alias-clj))

(defconst typed-clojure-check-form-str
  "(do (require 'clojure.core.typed)
     (if-let [res (seq (:delayed-errors (clojure.core.typed/check-form-info '%s)))]
        (for [^Exception e res]
          (let [{:keys [env] :as data} (ex-data e)]
           (.getMessage e)))
        (with-out-str (clojure.pprint/write (clojure.core.typed/cf %s)))))")

(defconst typed-clojure-clj-check-ns-code "
         (let [_ (require 'clojure.core.typed)
               check-ns-info (find-var 'clojure.core.typed/check-ns-info)
               _ (assert check-ns-info
                   \"clojure.core.typed/check-ns-info not found\")
               {:keys [delayed-errors]} (check-ns-info)]
            (if (seq delayed-errors)
                 (for [^Exception e delayed-errors]
                      (let [{:keys [env] :as data} (ex-data e)]
                        (list (.getMessage e) (:line env)
                        (:column env) (if (contains? data :form) (str (:form data)) 0)
                        (:file env) (-> env :ns :name str))))
              '()))")

(defun typed-clojure-make-check-form-handler (cb)
  (nrepl-make-response-handler
   cb
   (lambda (buffer val)
     (let ((inhibit-read-only t)
           (buffer-undo-list t)
           (rd (read val)))
       (if (listp rd)
           (with-current-buffer (cider-popup-buffer
                                 cider-error-buffer
                                 nil)
             (typed-clojure-error-mode)
             (goto-char (point-max))
             (insert "Type Error\n")
             (mapc (lambda (msg)
                     (insert (format "%s\n" msg)))
                   rd))
         (message ":ok"))))
   '()
   '()
   '()))

(defun typed-clojure-make-check-ns-handler (cb)
  (lexical-let ((cb cb))
    (nrepl-make-response-handler
     cb
     (lambda (buffer val)
       (let ((inhibit-read-only t)
	     (buffer-undo-list t)
	     (rd (read val)))
	 (if (= 0 (length rd))
	     (message ":ok")
	   (with-current-buffer (cider-popup-buffer
				 cider-error-buffer
				 nil)
	     (typed-clojure-error-mode)
	     (goto-char (point-max))
	     (mapcar
	      (lambda (x)
		(lexical-let ((msg    (first x))
			      (line   (second x))
			      (column (third x))
			      (form   (fourth x))
			      (source (fifth x))
			      (ns     (sixth x)))
		  (insert "Type Error (")
		  (insert-button (concat (or source "NO_SOURCE_FILE")
					 ":"
					 (format "%s:%s" line column))
				 'action
				 (lambda (y)
				   (switch-to-buffer cb)
				   (forward-line (- line (line-number-at-pos)))
				   (move-to-column column)))
		  (insert ")\n")
		  (insert (format "%s\n\n\n" msg)))) rd)))))
     '()
     '()
     '())))

(defun typed-clojure-check-ns ()
  "Type check and pretty print errors for the namespace."
  (interactive)
  (let ((cb (current-buffer)))
    (cider-tooling-eval typed-clojure-clj-check-ns-code
                        (typed-clojure-make-check-ns-handler cb)
                        (cider-current-ns))))

(defun typed-clojure-check-form (&optional prefix)
  "Typecheck the preceding form."
  (interactive "P")
  (let ((cb (current-buffer))
        (last-form (cider-last-sexp)))
    (cider-tooling-eval (format typed-clojure-check-form-str
                                last-form
                                last-form)
                        (typed-clojure-make-check-form-handler cb)
                        (cider-current-ns))))

(defun typed-clojure-ann-var ()
  (interactive)
  (lexical-let ((t (read-string "Annotate var with type (default Any): ")))
    (lexical-let ((sym (thing-at-point 'symbol)))
      (lexical-let ((p (typed-clojure-qualify-ann-var sym)))
        (if p
            (progn
              (beginning-of-defun)
              (insert "\n")
              (forward-line -1)
              (insert (format "(%sann " (typed-clojure-current-alias)))
              (insert (concat p " " (if (= 0 (length t)) "Any" t) ")"))
              ())
          (error (concat "Current form is not a symbol: " sym)))))
    (backward-sexp)))

(defun typed-clojure-ann-form ()
  (interactive)
  (lexical-let ((t (read-string "Annotate form with type (default Any): ")))
    (ignore-errors
      (forward-sexp)
      (backward-sexp))
    (save-excursion
      (insert (format "(%sann-form " (typed-clojure-current-alias)))
      (forward-sexp)
      (insert (format "%s)" (concat " " (if (= 0 (length t)) "Any" t))))
      (backward-up-list))
    (save-excursion
      (mark-defun))))

(provide 'typed-clojure-mode)

;;; typed-clojure-mode.el ends here
