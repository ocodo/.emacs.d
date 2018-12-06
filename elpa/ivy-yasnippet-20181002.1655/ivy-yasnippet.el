;;; ivy-yasnippet.el --- Preview yasnippets with ivy

;;
;; Author: Michał Krzywkowski <k.michal@zoho.com>
;; URL: https://github.com/mkcms/ivy-yasnippet
;; Package-Version: 20181002.1655
;; Package-Requires: ((emacs "24") (ivy "0.10.0") (yasnippet "0.12.2") (dash "2.14.1") (cl-lib))
;; Version: 0.0.1
;; Keywords: convenience

;; Copyright (C) 2018 Michał Krzywkowski

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
;; This package allows you to select yasnippet snippets using ivy completion.
;; When current selection changes in the minibuffer, the snippet contents
;; are temporarily expanded in the buffer.
;;
;; To use it, call M-x ivy-yasnippet (but make sure you have enabled
;; `yas-minor-mode' first).
;;

;;; Code:

(require 'ivy)
(require 'yasnippet)
(require 'dash)
(require 'cl-lib)

(defvar ivy-yasnippet--buffer nil)
(defvar ivy-yasnippet--template-alist nil)
(defvar ivy-yasnippet--region nil)
(defvar ivy-yasnippet--region-contents nil)
(defvar ivy-yasnippet--key nil)
(defvar ivy-yasnippet--key-deleted nil)
(defvar ivy-yasnippet--should-delete-key nil)

(defgroup ivy-yasnippet nil
  "Preview yasnippets with ivy."
  :group 'ivy
  :group 'yasnippet)

(defcustom ivy-yasnippet-expand-keys 'smart
  "Value that says how to expand keys before point.
If it's nil, never expand keys.
If it's the symbol `always`, always try to expand keys.
If it's the symbol `smart`, expand when a matching candidate is selected for
the first time.  Once a candidate whose key doesn't match whatever is before
point is selected, behave like nil until the minibuffer exits."
  :group 'ivy-yasnippet
  :type '(choice (const :tag "Never" nil)
		 (const :tag "Always" always)
		 (const :tag "Expand until a nonexpandable candidate\
 is selected" smart)))

(defcustom ivy-yasnippet-create-snippet-if-not-matched t
  "If non-nil, allow exiting the minibuffer without exact match.
Doing so will pop up a new buffer for writing a snippet."
  :group 'ivy-yasnippet
  :type 'boolean)

(defcustom ivy-yasnippet-new-snippet
  "# name: ${1:`name`}${2:
# key: ${3:key}}${4:
# keybinding: ${5:keybinding}}${6:
# expand-env: (${7:(var val)})}${8:
# contributor: $9}
# --
$0`yas-selected-text`"
  "Snippet to expand when creating new snippet.
During expansion, `name` is bound to whatever was returned by `ivy-read'."
  :group 'ivy-yasnippet
  :type 'string)

(defface ivy-yasnippet-key
  '((t . (:inherit font-lock-type-face)))
  "Face used for keys."
  :group 'ivy-yasnippet)

(defface ivy-yasnippet-key-matching
  '((t . (:inherit ivy-yasnippet-key :weight bold)))
  "Face used for keys that match whatever is before point."
  :group 'ivy-yasnippet)

(defun ivy-yasnippet--lookup-template (name)
  (cdr (assoc name ivy-yasnippet--template-alist)))

(defun ivy-yasnippet--revert ()
  (delete-region (car ivy-yasnippet--region)
		 (cdr ivy-yasnippet--region))
  (goto-char (car ivy-yasnippet--region))
  (when ivy-yasnippet--key-deleted
    (insert ivy-yasnippet--key)
    (setq ivy-yasnippet--key-deleted nil))
  (setcar ivy-yasnippet--region (point))
  (insert ivy-yasnippet--region-contents)
  (setcdr ivy-yasnippet--region (point)))

(defun ivy-yasnippet--expand-template (template)
  (deactivate-mark)
  (goto-char (car ivy-yasnippet--region))
  (cond
   ((not (string-equal "" ivy-yasnippet--region-contents))
    (push-mark (point))
    (push-mark (cdr ivy-yasnippet--region) nil t))
   ((and ivy-yasnippet--should-delete-key
	 (not ivy-yasnippet--key-deleted)
	 ivy-yasnippet--key
	 (string-equal ivy-yasnippet--key (yas--template-key template)))
    (let ((length (length ivy-yasnippet--key)))
      (delete-char (- length))
      (cl-decf (car ivy-yasnippet--region) length)
      (cl-decf (cdr ivy-yasnippet--region) length)
      (setq ivy-yasnippet--key-deleted t)))
   (t
    (setq ivy-yasnippet--should-delete-key
	  (eq ivy-yasnippet-expand-keys 'always))))
  (yas-expand-snippet (yas--template-content template)
		      nil nil
		      (yas--template-expand-env template)))

(defun ivy-yasnippet--preview (template)
  (with-current-buffer ivy-yasnippet--buffer
    (let ((yas-verbosity 0)
	  (inhibit-redisplay t)
	  (inhibit-read-only t)
	  (orig-offset (- (point-max) (cdr ivy-yasnippet--region)))
	  (yas-prompt-functions '(yas-no-prompt)))
      (ivy-yasnippet--revert)
      (unwind-protect
	  (ivy-yasnippet--expand-template template)
	(unwind-protect
	    (mapc #'yas--commit-snippet
		  (yas-active-snippets (point-min) (point-max)))
	  (setcdr ivy-yasnippet--region (- (point-max) orig-offset)))))
    (redisplay)))

(defun ivy-yasnippet--update-fn ()
  (let* ((candidate (ivy-state-current ivy-last))
	 (template (ivy-yasnippet--lookup-template candidate)))
    (when template
      (condition-case-unless-debug err
	  (ivy-yasnippet--preview template)
	(error (warn "ivy-yasnippet--update-fn: %S" err))))))

(defun ivy-yasnippet--visit-snippet-action (template-name)
  (let ((inhibit-read-only t))
    (ivy-yasnippet--revert))
  (yas--visit-snippet-file-1
   (ivy-yasnippet--lookup-template template-name)))

;;;###autoload
(defun ivy-yasnippet ()
  "Read a snippet name from the minibuffer and expand it at point.
The completion is done using `ivy-read'.

In the minibuffer, each time selection changes, the selected
snippet is temporarily expanded at point for preview.

If text before point matches snippet key of any candidate, that
candidate will be initially selected, unless variable
`ivy-yasnippet-expand-keys' is set to nil."
  (interactive)
  (barf-if-buffer-read-only)
  (unless yas-minor-mode
    (error "yas-minor-mode not enabled in current buffer"))
  (let* ((ivy-yasnippet--buffer (current-buffer))

	 (ivy-yasnippet--region
	  (if (region-active-p)
	      (cons (region-beginning) (region-end))
	    (cons (point) (point))))
	 (ivy-yasnippet--region-contents
	  (buffer-substring (car ivy-yasnippet--region)
			    (cdr ivy-yasnippet--region)))

	 (key-info (yas--templates-for-key-at-point))
	 (ivy-yasnippet--key
	  (and key-info
	       (buffer-substring (cadr key-info) (cl-caddr key-info))))
	 (templates-for-key-at-point (mapcar #'cdr (car key-info)))
	 (ivy-yasnippet--key-deleted nil)
	 (ivy-yasnippet--should-delete-key
	  (memq ivy-yasnippet-expand-keys '(always smart)))

	 (ivy-yasnippet--template-alist
	  (mapcar (lambda (template)
		    (cons (yas--template-name template) template))
		  (yas--all-templates (yas--get-snippet-tables))))

	 (modified-flag (buffer-modified-p))

	 candidates selection)
    (let ((buffer-undo-list t))
      (setq candidates
	    (-map #'car
		  (-flatten
		   (-map (-partial #'-sort (lambda (a b)
					     (string-lessp (car a) (car b))))
			 (-separate
			  (-lambda ((_ . template))
			    (memq template templates-for-key-at-point))
			  ivy-yasnippet--template-alist)))))

      (unwind-protect
	  (let ((buffer-read-only t))
	    (ivy-read "Choose a snippet: " candidates
		      :require-match
		      (not ivy-yasnippet-create-snippet-if-not-matched)
		      :update-fn #'ivy-yasnippet--update-fn
		      :action (lambda (candidate) (setq selection candidate))
		      :preselect
		      (when ivy-yasnippet--key
			(-find-index
			 (lambda (x)
			   (string-equal
			    ivy-yasnippet--key
			    (yas--template-key
			     (ivy-yasnippet--lookup-template x))))
			 candidates))
		      :caller 'ivy-yasnippet))
	(ivy-yasnippet--revert)
	(set-buffer-modified-p modified-flag)))
    (when selection
      (let ((template (ivy-yasnippet--lookup-template selection)))
	(if template
	    (ivy-yasnippet--expand-template template)
	  (setq template ivy-yasnippet-new-snippet)
	  (yas-new-snippet t)
	  (when (derived-mode-p 'snippet-mode)
	    (yas-expand-snippet
	     template nil nil
	     `((name ,selection)
	       (yas-selected-text
		,ivy-yasnippet--region-contents)))))))))

(defun ivy-yasnippet-transformer (template-name)
  (let* ((template (ivy-yasnippet--lookup-template template-name))
	 (key (yas--template-key template)))
    (if key
	(concat template-name
		" " (propertize (concat "[" key "]")
			    'face
			    (if (and (string-equal key ivy-yasnippet--key)
				     ivy-yasnippet--should-delete-key)
				'ivy-yasnippet-key-matching
			      'ivy-yasnippet-key)))
      template-name)))

(ivy-set-display-transformer 'ivy-yasnippet
			     #'ivy-yasnippet-transformer)

(ivy-add-actions
 #'ivy-yasnippet
 '(("v" ivy-yasnippet--visit-snippet-action "Visit snippet file")))

(provide 'ivy-yasnippet)

;;; ivy-yasnippet.el ends here
