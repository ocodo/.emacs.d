;;; auto-complete-nxml.el --- do completion by auto-complete.el on nXML-mode

;; Copyright (C) 2013  Hiroaki Otsu

;; Author: Hiroaki Otsu <ootsuhiroaki@gmail.com>
;; Keywords: completion, html, xml
;; URL: https://github.com/aki2o/auto-complete-nxml
;; Package-Requires: ((auto-complete "1.4"))
;; Version: 20140220.2058
;; X-Original-Version: 0.5.0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; 
;; This extension provides completion by auto-complete.el on nXML-mode.
;; About auto-complete.el, see <https://github.com/auto-complete/auto-complete>.

;;; Dependency:
;; 
;; - Following have been installed, auto-complete.el, auto-complete-config.el.
;; - nXML-mode is available.

;;; Installation:
;;
;; Put this to your load-path.
;; And put the following lines in your .emacs or site-start.el file.
;; 
;; (require 'auto-complete-nxml)

;;; Configuration:
;; 
;; ;; Keystroke for popup help about something at point.
;; (setq auto-complete-nxml-popup-help-key "C-:")
;; 
;; ;; Keystroke for toggle on/off automatic completion.
;; (setq auto-complete-nxml-toggle-automatic-key "C-c C-t")
;; 
;; ;; If you want to start completion manually from the beginning
;; (setq auto-complete-nxml-automatic-p nil)

;;; Customization:
;; 
;; [EVAL] (autodoc-document-lisp-buffer :type 'user-variable :prefix "auto-complete-nxml-[^\-]" :docstring t)
;; `auto-complete-nxml-popup-help-key'
;; Keystroke for popup help about something at point.
;; `auto-complete-nxml-toggle-automatic-key'
;; Keystroke for toggle on/off automatic completion.
;; `auto-complete-nxml-automatic-p'
;; Whether start completion automatically.
;; 
;;  *** END auto-documentation

;;; API:
;; 
;; [EVAL] (autodoc-document-lisp-buffer :type 'command :prefix "auto-complete-nxml-[^\-]" :docstring t)
;; `auto-complete-nxml-ac-start-with-insert'
;; Not documented.
;; `auto-complete-nxml-popup-help'
;; Popup help about something at point.
;; `auto-complete-nxml-toggle-automatic'
;; Switch value of `auto-complete-nxml-automatic-p'.
;; 
;;  *** END auto-documentation
;; [Note] Functions and variables other than listed above, Those specifications may be changed without notice.

;;; Tested On:
;; 
;; - Emacs ... GNU Emacs 23.3.1 (i386-mingw-nt5.1.2600) of 2011-08-15 on GNUPACK
;; - auto-complete.el ... Version 1.4
;; - auto-complete-config.el ... Version 1.4


;; Enjoy!!!


(eval-when-compile (require 'cl))
(require 'rx)
(require 'regexp-opt)
(require 'nxml-util)
(require 'rng-nxml)
(require 'rng-loc)
(require 'auto-complete)
(require 'auto-complete-config)
(require 'anything-project nil t)
(require 'thingatpt)
(require 'pos-tip nil t)


(defgroup auto-complete-nxml nil
  "Auto completion for nXML-mode."
  :group 'completion
  :prefix "auto-complete-nxml-")

(defcustom auto-complete-nxml-popup-help-key nil
  "Keystroke for popup help about something at point."
  :type 'string
  :group 'auto-complete-nxml)

(defcustom auto-complete-nxml-toggle-automatic-key nil
  "Keystroke for toggle on/off automatic completion."
  :type 'string
  :group 'auto-complete-nxml)

(defcustom auto-complete-nxml-automatic-p t
  "Whether start completion automatically."
  :type 'boolean
  :group 'auto-complete-nxml)


(defsubst auto-complete-nxml-start-completion-p ()
  (or auto-complete-nxml-automatic-p
      (eq this-command 'ac-trigger-key-command)))


(defadvice rng-set-document-type-and-validate (around make-doc4ac-in-nxml activate)
  (let* ((startp (auto-complete-nxml-start-make-doc4ac-in-nxml)))
    (when startp
      (auto-complete-nxml-enable-make-doc4ac-in-nxml))
    ad-do-it
    (when startp
      (auto-complete-nxml-disable-make-doc4ac-in-nxml))))

(defvar auto-complete-nxml-note-stored-index 0)
(defvar auto-complete-nxml-note-store-hash (make-hash-table))
(defvar auto-complete-nxml-ncls-stored-index 0)
(defvar auto-complete-nxml-ncls-store-hash (make-hash-table))
(defvar auto-complete-nxml-element-document-hash (make-hash-table :test 'equal))
(defvar auto-complete-nxml-attribute-document-hash (make-hash-table :test 'equal))
(defstruct auto-complete-nxml-doc name ns comment note)
(defun auto-complete-nxml-start-make-doc4ac-in-nxml ()
  (setq auto-complete-nxml-note-stored-index 0)
  (setq auto-complete-nxml-note-store-hash (make-hash-table))
  (setq auto-complete-nxml-ncls-stored-index 0)
  (setq auto-complete-nxml-ncls-store-hash (make-hash-table))
  (setq auto-complete-nxml-element-document-hash (make-hash-table :test 'equal))
  (setq auto-complete-nxml-attribute-document-hash (make-hash-table :test 'equal))
  t)

(defun auto-complete-nxml-enable-make-doc4ac-in-nxml ()
  (ad-enable-regexp (format "\\`%s\\'" 'auto-complete-nxml-ad-make-doc))
  (auto-complete-nxml-ad-activate))

(defun auto-complete-nxml-disable-make-doc4ac-in-nxml ()
  (ad-disable-regexp (format "\\`%s\\'" 'auto-complete-nxml-ad-make-doc))
  (auto-complete-nxml-ad-activate))

(defun auto-complete-nxml-ad-activate ()
  (loop for s in '(rng-c-parse-element
                   rng-c-parse-attribute
                   rng-c-parse-name-class
                   forward-comment
                   rng-c-parse-follow-annotations)
        do (ad-activate s)))

(defun auto-complete-nxml-get-note-stored-index ()
  (incf auto-complete-nxml-note-stored-index))
(defun auto-complete-nxml-get-stored-note (idx)
  (gethash idx auto-complete-nxml-note-store-hash))
(defun auto-complete-nxml-store-note (note)
  (let* ((currnote (gethash auto-complete-nxml-note-stored-index auto-complete-nxml-note-store-hash))
         (newnote (cond (currnote (concat currnote "\n" note))
                        (t        note))))
    (puthash auto-complete-nxml-note-stored-index newnote auto-complete-nxml-note-store-hash)))

(defun auto-complete-nxml-get-ncls-stored-index ()
  (incf auto-complete-nxml-ncls-stored-index))
(defun auto-complete-nxml-get-stored-ncls (idx)
  (gethash idx auto-complete-nxml-ncls-store-hash))
(defun auto-complete-nxml-store-ncls (ncls)
  (puthash auto-complete-nxml-ncls-stored-index ncls auto-complete-nxml-ncls-store-hash))

(defadvice rng-c-parse-element (around auto-complete-nxml-ad-make-doc disable)
  (let* ((nclsidx (auto-complete-nxml-get-ncls-stored-index))
         (noteidx (auto-complete-nxml-get-note-stored-index))
         (comment auto-complete-nxml-current-schema-comment))
    (setq auto-complete-nxml-current-schema-comment "")
    ad-do-it
    (auto-complete-nxml-make-document nclsidx noteidx comment auto-complete-nxml-element-document-hash)))

(defadvice rng-c-parse-attribute (around auto-complete-nxml-ad-make-doc disable)
  (let* ((nclsidx (auto-complete-nxml-get-ncls-stored-index))
         (noteidx (auto-complete-nxml-get-note-stored-index))
         (comment auto-complete-nxml-current-schema-comment))
    (setq auto-complete-nxml-current-schema-comment "")
    ad-do-it
    (auto-complete-nxml-make-document nclsidx noteidx comment auto-complete-nxml-attribute-document-hash)))

(defun auto-complete-nxml-make-document (nclsidx noteidx comment hash)
  (let* ((name-class (auto-complete-nxml-get-stored-ncls nclsidx))
         (typesym (when name-class (pop name-class)))
         (typecons (when (listp name-class) (pop name-class)))
         (nssym (when typecons (car typecons)))
         (ns (or (when nssym (nxml-namespace-name nssym))
                 ""))
         (currnm (or (when typecons (cdr typecons))
                     ""))
         (key (cond ((and (not (string= ns ""))
                          (not (string= currnm "")))
                     (concat ns ":" currnm))
                    ((not (string= currnm ""))
                     currnm)))
         (note (or (auto-complete-nxml-get-stored-note noteidx)
                   ""))
         (doc (when (stringp key)
                (make-auto-complete-nxml-doc :name currnm :ns ns :comment comment :note note))))
    (when (auto-complete-nxml-doc-p doc)
      (puthash key doc hash))))

(defadvice rng-c-parse-name-class (after auto-complete-nxml-ad-make-doc disable)
  (auto-complete-nxml-store-ncls ad-return-value))

(defvar auto-complete-nxml-current-schema-comment "")
(defadvice forward-comment (around auto-complete-nxml-ad-make-doc disable)
  (let* ((startpt (point)))
    ad-do-it
    (let* ((text (buffer-substring-no-properties startpt (point)))
           (text (replace-regexp-in-string "^\\s-+" "" text))
           (text (replace-regexp-in-string "\\s-+$" "" text))
           (text (replace-regexp-in-string "^#+" "" text))
           (text (replace-regexp-in-string "^\\s-+" "" text))
           (text (replace-regexp-in-string "[\r\n]+" "\n" text))
           (text (replace-regexp-in-string "\\`[\r\n]+" "" text))
           (text (replace-regexp-in-string "[\r\n]+\\'" "" text))
           (curr auto-complete-nxml-current-schema-comment))
      (when (not (string= text ""))
        (setq auto-complete-nxml-current-schema-comment (cond ((not (string= curr "")) (concat curr "\n" text))
                                                              (t                       text)))))))

(defvar auto-complete-nxml-regexp-rnc-annotation (rx-to-string `(and bos (* space) "a:documentation" (+ space) "[" )))
(defadvice rng-c-parse-follow-annotations (around auto-complete-nxml-ad-make-doc disable)
  (let* ((startpt (point)))
    ad-do-it
    (let* ((text (buffer-substring-no-properties startpt (point)))
           (text (replace-regexp-in-string "\\`\\s-*a:documentation\\s-+\\[\\s-*" "" text))
           (note ""))
      (loop for line in (split-string text "\r?\n")
            for line = (replace-regexp-in-string "^\\s-+" "" line)
            if (string-match "^\"" line)
            do (let* ((line (replace-regexp-in-string "^\"" "" line))
                      (line (replace-regexp-in-string "\"\\s-*~?\\s-*$" "" line))
                      (line (replace-regexp-in-string ".$" "" line)) ; delete special character at end of line
                      (line (replace-regexp-in-string "^\\s-+" "" line))
                      (line (replace-regexp-in-string "\\s-+$" "" line)))
                 (when (not (string= line ""))
                   (when (not (string= note ""))
                     (setq note (concat note "\n")))
                   (setq note (concat note line)))))
      (when (not (string= note ""))
        (auto-complete-nxml-store-note note)))))


(defvar auto-complete-nxml-regexp-jump-current-tag-start (rx-to-string `(and (group (or "<" ">")) (* (not (any ">"))))))
(defun auto-complete-nxml-point-inside-tag-p ()
  (save-excursion
    (and (re-search-backward auto-complete-nxml-regexp-jump-current-tag-start nil t)
         (string= (match-string-no-properties 1) "<"))))

(defvar auto-complete-nxml-buffer-current-tag nil)
(make-variable-buffer-local 'auto-complete-nxml-buffer-current-tag)
(defun auto-complete-nxml-update-current-tag ()
  (save-excursion
    (let* ((tagnm ""))
      (when (re-search-backward "<[^/]" nil t)
        (forward-char)
        (let* ((start (point)))
          (skip-syntax-forward "w")
          (setq tagnm (buffer-substring-no-properties start (point)))))
      (setq auto-complete-nxml-buffer-current-tag tagnm))))

(defvar auto-complete-nxml-regexp-point-inside-attr (rx-to-string `(and (+ space) (group (+ (not (any space)))) "=" (or "\"" "'")
                                                       (* (not (any "\"" "'"))) point)))
(defvar auto-complete-nxml-buffer-current-attr nil)
(make-variable-buffer-local 'auto-complete-nxml-buffer-current-attr)
(defun auto-complete-nxml-update-current-attr ()
  (let* ((attrnm ""))
    (when (auto-complete-nxml-point-inside-tag-p)
      (save-excursion
        (when (re-search-backward auto-complete-nxml-regexp-point-inside-attr nil t)
          (setq attrnm (match-string-no-properties 1)))))
    (setq auto-complete-nxml-buffer-current-attr attrnm)))

(defvar auto-complete-nxml-regexp-point-tagnm (rx-to-string `(and "<" (* (not (any "/" ">" space))) point)))
(defvar auto-complete-nxml-regexp-point-attrnm (rx-to-string `(and (or (and "<" (+ (any "a-zA-Z0-9:-")))
                                                           (and (not (any "=")) "\"")
                                                           (and (not (any "=")) "'"))
                                                       (+ space) (* (any "a-zA-Z0-9-")) point)))
(defvar auto-complete-nxml-regexp-point-cssprop (rx-to-string `(and (or "\"" "'" ";") (* space) (* (any "a-zA-Z0-9-")) point)))
(defvar auto-complete-nxml-regexp-point-cssprop-value (rx-to-string `(and (or "\"" "'" ";" space) (group (+ (any "a-zA-Z0-9-"))) ":" (* space)
                                                              (* (not (any ":" "\"" "'"))) point)))
(defun auto-complete-nxml-get-css-candidates ()
  (ignore-errors
    (save-excursion
      (when (and (auto-complete-nxml-start-completion-p)
                 (not (re-search-backward auto-complete-nxml-regexp-point-tagnm nil t))
                 (auto-complete-nxml-update-current-attr)
                 (string= auto-complete-nxml-buffer-current-attr "style"))
        (cond ((re-search-backward auto-complete-nxml-regexp-point-cssprop nil t)
               (loop for prop in ac-css-property-alist
                     collect (car prop)))
              (t
               (ac-css-property-candidates)))))))

(defun auto-complete-nxml-get-project-ident (&optional projid)
  (or projid
      (and (featurep 'anything-project)
           (functionp 'ap:get-root-directory)
           (ap:get-root-directory))
      "default"))

(defvar auto-complete-nxml-tag-value-words-hash (make-hash-table :test 'equal))
(defun auto-complete-nxml-get-project-tag-value-words (&optional projid)
  (gethash (auto-complete-nxml-get-project-ident projid) auto-complete-nxml-tag-value-words-hash))
(defun auto-complete-nxml-put-project-tag-value-words (words &optional projid)
  (puthash (auto-complete-nxml-get-project-ident projid) words auto-complete-nxml-tag-value-words-hash))

(defvar auto-complete-nxml-attr-words-hash-hash (make-hash-table :test 'equal))
(defun auto-complete-nxml-get-project-attr-words-hash (&optional projid)
  (gethash (auto-complete-nxml-get-project-ident projid) auto-complete-nxml-attr-words-hash-hash))
(defun auto-complete-nxml-put-project-attr-words-hash (words-hash &optional projid)
  (puthash (auto-complete-nxml-get-project-ident projid) words-hash auto-complete-nxml-attr-words-hash-hash))

(defvar auto-complete-nxml-regexp-tag-value (rx-to-string `(and ">" (group (+ (not (any "<")))) "<")))
(defun auto-complete-nxml-update-tag-value-words (&optional projid)
  (save-excursion
    (loop initially (goto-char (point-min))
          with words = (auto-complete-nxml-get-project-tag-value-words projid)
          while (re-search-forward auto-complete-nxml-regexp-tag-value nil t)
          for text = (match-string-no-properties 1)
          do (loop for w in (split-string text "\\(?:^\\|\\_>\\).*?\\(?:\\_<\\|$\\)")
                   if (not (string= w ac-prefix))
                   do (add-to-list 'words w))
          finally (auto-complete-nxml-put-project-tag-value-words words projid))))

(defun auto-complete-nxml-get-tag-value-candidates-by-myself ()
  (ignore-errors
    (when (and (auto-complete-nxml-start-completion-p)
               (not (auto-complete-nxml-point-inside-tag-p)))
      (auto-complete-nxml-update-tag-value-words)
      (auto-complete-nxml-get-project-tag-value-words))))

(defun auto-complete-nxml-get-tag-value-candidates-by-nxml ()
  (ignore-errors
    (when (and (auto-complete-nxml-start-completion-p)
               (not (auto-complete-nxml-point-inside-tag-p)))
      (rng-match-save
        (rng-set-state-after)
        (rng-match-possible-value-strings)))))

(defvar auto-complete-nxml-regexp-attr (rx-to-string `(and (group (+ (any "a-zA-Z0-9-"))) "="
                                                           (or "\"" "'") (group (+ (not (any "\"" "'")))) (or "\"" "'"))))
(defun auto-complete-nxml-update-attr-words (&optional projid)
  (save-excursion
    (loop initially (goto-char (point-min))
          with words-hash = (or (auto-complete-nxml-get-project-attr-words-hash projid)
                                (make-hash-table :test 'equal))
          while (re-search-forward auto-complete-nxml-regexp-attr nil t)
          for attrnm = (match-string-no-properties 1)
          for attrvalue = (match-string-no-properties 2)
          if (and (auto-complete-nxml-point-inside-tag-p)
                  (not (string= attrnm "style"))
                  (not (string= attrnm "id")))
          do (let* ((words (gethash attrnm words-hash)))
               (loop for w in (split-string attrvalue)
                     if (not (string= w ac-prefix))
                     do (add-to-list 'words w))
               (puthash attrnm words words-hash))
          finally (auto-complete-nxml-put-project-attr-words-hash words-hash projid))))

(defun auto-complete-nxml-get-attr-value-candidates ()
  (ignore-errors
    (or (auto-complete-nxml-get-candidates)
        (when (auto-complete-nxml-start-completion-p)
          (auto-complete-nxml-update-current-attr)
          (let ((attrnm auto-complete-nxml-buffer-current-attr))
            (when (and (not (string= attrnm ""))
                       (not (string= attrnm "style"))
                       (not (string= attrnm "id")))
              (auto-complete-nxml-update-attr-words)
              (gethash attrnm (auto-complete-nxml-get-project-attr-words-hash))))))))

(defvar auto-complete-nxml-candidates nil)
(defun auto-complete-nxml-get-candidates ()
  (when (auto-complete-nxml-start-completion-p)
    (let ((auto-complete-nxml-candidates))
      (flet ((rng-complete-before-point (start table prompt &optional predicate hist)
                                        (let ((inputw (buffer-substring-no-properties start (point))))
                                          (setq auto-complete-nxml-candidates
                                                (cond ((functionp table)
                                                       (funcall table inputw nil t))
                                                      ((listp table)
                                                       (loop for e in table
                                                             collect (car e)))))
                                          nil)))
        (ignore-errors (rng-complete))
        (loop with h = (make-hash-table :test 'equal)
              for c in auto-complete-nxml-candidates
              if (not (gethash c h))
              collect (puthash c c h))))))

(defun auto-complete-nxml-expand-tag ()
  (let* ((currpt (point))
         (tagnm (save-excursion
                  (skip-syntax-backward "w")
                  (buffer-substring-no-properties (point) currpt)))
         (qname))
    (cond ((rng-qname-p tagnm)
           (setq qname (rng-expand-qname tagnm t 'rng-start-tag-expand-recover))
           (when (and qname
                      (rng-match-start-tag-open qname)
                      (or (not (rng-match-start-tag-close))
                          (and (car qname)
                               (not rng-open-elements))))
             (insert " ")))
          ((member tagnm rng-complete-extra-strings)
           (insert ">")))))

(defvar auto-complete-nxml-regexp-point-expand-xmlns (rx-to-string `(and "xmlns="
                                                                         (group (or "\"" "'"))
                                                                         (group (+ (not (any "\"" "'"))))
                                                                         point)))
(defun auto-complete-nxml-expand-other-xmlns ()
  (when (save-excursion
          (re-search-backward auto-complete-nxml-regexp-point-expand-xmlns nil t))
    (when (and (not (= (point) (point-max)))
               (string= (format "%c" (char-after)) "\""))
      (delete-char 1))
    (insert "\"")
    (let* ((defns (match-string-no-properties 2)))
      (loop for nssym in (rng-match-possible-namespace-uris)
            for ns = (nxml-namespace-name nssym)
            for prefix = (when (not (string= ns defns))
                           (auto-complete-nxml-get-prefix ns))
            if (and prefix
                    (not (string= prefix "")))
            do (progn (cond (indent-tabs-mode
                             (insert "\n")
                             (indent-for-tab-command))
                            (t
                             (insert " ")))
                      (insert (format "xmlns:%s=\"%s\"" prefix ns)))))))

(defun auto-complete-nxml-get-prefix (ns)
  (loop for f in rng-schema-locating-files
        for id = (loop for rule in (rng-get-parsed-schema-locating-file f)
                       for nscons = (when (eq (car rule) 'namespace)
                                      (assq 'ns (cdr rule)))
                       for idcons = (when (and nscons
                                               (string= (cdr nscons) ns))
                                      (assq 'typeId (cdr rule)))
                       if idcons return (cdr idcons)
                       finally return nil)
        if id
        return (loop for rule in (rng-get-parsed-schema-locating-file f)
                     for idcons = (when (eq (car rule) 'documentElement)
                                    (assq 'typeId (cdr rule)))
                     for prefcons = (when (and idcons
                                               (string= (cdr idcons) id))
                                      (assq 'prefix (cdr rule)))
                     if prefcons return (cdr prefcons)
                     finally return nil)
        finally return nil))

(defun auto-complete-nxml-get-document-tag (selected)
  (auto-complete-nxml-get-document-selected selected auto-complete-nxml-element-document-hash "ELEMENT"))

(defun auto-complete-nxml-get-document-attr (selected)
  (auto-complete-nxml-get-document-selected selected auto-complete-nxml-attribute-document-hash "ATTRIBUTE"))

(defun auto-complete-nxml-get-document-selected (selected stored-hash typenm)
  (ignore-errors
    (if (not (stringp selected))
        ""
      (set-text-properties 0 (string-width selected) nil selected)
      (with-temp-buffer
        (let* ((standard-output (current-buffer))
               (currnm selected)
               (prefix (when (string-match ":" selected)
                         (let* ((e (split-string selected ":")))
                           (when (= (length e) 2)
                             (setq currnm (nth 1 e))
                             (nth 0 e)))))
               (nssym (cond ((and (stringp prefix)
                                  (not (string= prefix "")))
                             (nxml-ns-get-prefix prefix))
                            (t
                             (nxml-ns-get-default))))
               (ns (or (when nssym (nxml-namespace-name nssym))
                       ""))
               (key (cond ((not (string= ns "")) (concat ns ":" currnm))
                          (t                     currnm)))
               (doc (when (stringp key) (gethash key stored-hash)))
               (comment (or (when (auto-complete-nxml-doc-p doc)
                              (auto-complete-nxml-doc-comment doc))
                            ""))
               (note (or (when (auto-complete-nxml-doc-p doc)
                           (auto-complete-nxml-doc-note doc))
                         "")))
          (when (and (not (string= currnm ""))
                     (not (string-match "^/" currnm)))
            (princ (format "'%s' is %s in '%s'.\n" currnm typenm ns))
            (cond ((and (string= comment "")
                        (string= note ""))
                   (princ "\n")
                   (princ "Not documented.\n"))
                  (t
                   (when (not (string= comment ""))
                     (princ "\n")
                     (princ (format "Comment: \n%s\n" comment)))
                   (when (not (string= note ""))
                     (princ "\n")
                     (princ (format "Note: \n%s\n" note))))))
          (buffer-string))))))

(defun auto-complete-nxml-get-current-context-symbol ()
  (ignore-errors
    (save-excursion
      (cond ((re-search-backward auto-complete-nxml-regexp-point-tagnm nil t)
             ;; point on tag
             (setq auto-complete-nxml-buffer-current-tag
                   (buffer-substring-no-properties (progn (forward-char 1)
                                                          (point))
                                                   (progn (re-search-forward "[ >]" nil t)
                                                          (forward-char -1)
                                                          (point))))
             'tag)
            ((auto-complete-nxml-point-inside-tag-p)
             ;; point on inside tag
             (auto-complete-nxml-update-current-tag)
             (cond ((save-excursion
                      (re-search-backward auto-complete-nxml-regexp-point-attrnm nil t))
                    ;; point on attribute
                    (setq auto-complete-nxml-buffer-current-attr
                          (buffer-substring-no-properties (progn (re-search-backward "\\s-" nil t)
                                                                 (forward-char 1)
                                                                 (point))
                                                          (progn (search-forward "=" nil t)
                                                                 (forward-char -1)
                                                                 (point))))
                    'attr)
                   ((and (auto-complete-nxml-update-current-attr)
                         (not (string= auto-complete-nxml-buffer-current-attr "")))
                    ;; point on value of attribute
                    (cond ((string= auto-complete-nxml-buffer-current-attr "style")
                           (cond ((re-search-backward auto-complete-nxml-regexp-point-cssprop nil t)
                                  'cssprop)
                                 (t
                                  'csspropvalue)))
                          (t
                           'attrvalue)))
                   (t
                    'otherwise)))
            (t
             ;; point on outside tag
             (cond ((and (auto-complete-nxml-update-current-tag)
                         (not (string= auto-complete-nxml-buffer-current-tag "")))
                    'content)
                   (t
                    'otherwise)))))))


(defvar auto-complete-nxml-regexp-point-endtag (rx-to-string `(and "</" (+ (not (any space))) point)))
(defvar ac-source-nxml-tag
  '((candidates . auto-complete-nxml-get-candidates)
    (prefix . "<\\([a-zA-Z0-9:-]*\\)")
    (symbol . "t")
    (document . auto-complete-nxml-get-document-tag)
    (requires . 0)
    (cache)
    (limit . 500)
    (action . (lambda ()
                (auto-complete-nxml-expand-tag)
                (when (or (= (point) (point-max))
                          (not (string= (format "%c" (char-after)) ">")))
                  (when (save-excursion
                          (re-search-backward auto-complete-nxml-regexp-point-endtag nil t))
                    (insert ">")))))))

(defvar ac-source-nxml-attr
  '((candidates . auto-complete-nxml-get-candidates)
    (prefix . "\\(?:<[a-zA-Z0-9:-]+\\|[^=]\"\\|[^=]'\\)\\s-+\\([a-zA-Z0-9-]*\\)")
    (symbol . "a")
    (document . auto-complete-nxml-get-document-attr)
    (requires . 0)
    (cache)
    (limit . 500)
    (action . (lambda ()
                (insert "=\"")
                (when (and (not (= (point) (point-max)))
                           (string= (format "%c" (char-after)) "\""))
                  (delete-char 1))
                (insert "\"")
                (backward-char)
                (when auto-complete-nxml-automatic-p
                  (auto-complete))))))

(defvar ac-source-nxml-attr-value
  '((candidates . auto-complete-nxml-get-attr-value-candidates)
    (prefix . "=\\(?:\"\\|'\\)\\s-*\\([^\"':; ]*\\)")
    (symbol . "v")
    (requires . 0)
    (cache)
    (limit . 500)
    (action . (lambda ()
                (auto-complete-nxml-expand-other-xmlns)))))

(defvar ac-source-nxml-css
  '((candidates . auto-complete-nxml-get-css-candidates)
    (prefix . "\\s-+style=\\(?:\"\\|'\\)\\([^\"']*\\)")
    (symbol . "c")
    (requires . 0)
    (cache)
    (limit . 500)
    (action . (lambda ()
                (insert ": ")
                (when auto-complete-nxml-automatic-p
                  (auto-complete '(ac-source-nxml-css-property)))))))

(defvar ac-source-nxml-css-property
  '((candidates . auto-complete-nxml-get-css-candidates)
    (prefix . ac-css-prefix)
    (symbol . "p")
    (requires . 0)
    (cache)
    (limit . 500)
    (action . (lambda ()
                (insert ";")))))

(defvar ac-source-nxml-tag-value-by-nxml
  '((candidates . auto-complete-nxml-get-tag-value-candidates-by-nxml)
    (prefix . ">\\s-*\\([^<]*\\)")
    (symbol . "w")
    (requires . 0)
    (cache)
    (limit . 500)
    (action . (lambda ()
                (let* ((startp (point))
                       (tstartp (when (search-backward "<" nil t)
                                  (forward-char 1)
                                  (point)))
                       (tagnm (and tstartp
                                   (when (re-search-forward "[ >]" nil t)
                                     (forward-char -1)
                                     (buffer-substring-no-properties tstartp (point)))))
                       (re (when tagnm
                             (rx-to-string `(and point (* space) "</" ,tagnm ">")))))
                  (goto-char startp)
                  (when (and tagnm
                             (save-excursion
                               (not (re-search-forward re nil t))))
                    (insert "</" tagnm ">")))))))

(defvar ac-source-nxml-tag-value-by-myself
  '((candidates . auto-complete-nxml-get-tag-value-candidates-by-myself)
    (symbol . "w")
    (cache)
    (limit . 500)))


(defun auto-complete-nxml-init-project ()
  (when (and (featurep 'anything-project)
             (functionp 'ap:get-project-files)
             (functionp 'ap:expand-file)
             (not (auto-complete-nxml-get-project-attr-words-hash)))
    (loop with projid = (auto-complete-nxml-get-project-ident)
          for f in (ap:get-project-files)
          for f = (ap:expand-file f)
          for mode = (assoc-default f auto-mode-alist 'string-match)
          if (eq mode 'nxml-mode)
          do (with-temp-buffer
               (insert-file-contents f nil)
               (auto-complete-nxml-update-tag-value-words projid)
               (auto-complete-nxml-update-attr-words projid)))))

(defun auto-complete-nxml-setup ()
  ;; Key binding
  (local-set-key (kbd "SPC") 'auto-complete-nxml-ac-start-with-insert)
  (when (and (stringp auto-complete-nxml-popup-help-key)
             (not (string= auto-complete-nxml-popup-help-key "")))
    (local-set-key (read-kbd-macro auto-complete-nxml-popup-help-key) 'auto-complete-nxml-popup-help))
  (when (and (stringp auto-complete-nxml-toggle-automatic-key)
             (not (string= auto-complete-nxml-toggle-automatic-key "")))
    (local-set-key (read-kbd-macro auto-complete-nxml-toggle-automatic-key) 'auto-complete-nxml-toggle-automatic))
  ;; Add auto-complete source
  (setq ac-sources '(ac-source-nxml-tag
                     ac-source-nxml-attr
                     ac-source-nxml-attr-value
                     ac-source-nxml-css
                     ac-source-nxml-css-property
                     ac-source-nxml-tag-value-by-nxml
                     ac-source-nxml-tag-value-by-myself))
  ;; Add mode enable auto-complete
  (add-to-list 'ac-modes 'nxml-mode)
  ;; Add trigger of auto-complete
  (add-to-list 'ac-trigger-commands 'auto-complete-nxml-ac-start-with-insert)
  (auto-complete-mode t)
  (auto-complete-nxml-init-project))

(add-hook 'nxml-mode-hook 'auto-complete-nxml-setup t)


(defun auto-complete-nxml-ac-start-with-insert (n)
  (interactive "p")
  (self-insert-command n)
  (when auto-complete-nxml-automatic-p
    (auto-complete-1 :triggered 'trigger-key)))

(defun auto-complete-nxml-popup-help ()
  "Popup help about something at point."
  (interactive)
  (let* ((ctx (auto-complete-nxml-get-current-context-symbol))
         (doc (case ctx
                (tag  (auto-complete-nxml-get-document-selected auto-complete-nxml-buffer-current-tag
                                                                auto-complete-nxml-element-document-hash
                                                                "ELEMENT"))
                (attr (auto-complete-nxml-get-document-selected auto-complete-nxml-buffer-current-attr
                                                                auto-complete-nxml-attribute-document-hash
                                                                "ATTRIBUTE")))))
    (if (and (functionp 'ac-quick-help-use-pos-tip-p)
             (ac-quick-help-use-pos-tip-p))
        (pos-tip-show doc 'popup-tip-face nil nil 300 popup-tip-max-width)
      (popup-tip doc))))

(defun auto-complete-nxml-toggle-automatic ()
  "Switch value of `auto-complete-nxml-automatic-p'."
  (interactive)
  (setq auto-complete-nxml-automatic-p (not auto-complete-nxml-automatic-p))
  (if auto-complete-nxml-automatic-p
      (message "Enabled auto-complete-nxml to be automatic.")
    (message "Disabled auto-complete-nxml to be automatic.")))


;; For compatibility
(defalias 'auto-complete-nxml-insert-with-ac-trigger-command 'auto-complete-nxml-ac-start-with-insert)


(provide 'auto-complete-nxml)
;;; auto-complete-nxml.el ends here
