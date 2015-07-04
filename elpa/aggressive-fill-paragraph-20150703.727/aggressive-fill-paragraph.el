;;; aggressive-fill-paragraph.el --- A mode to automatically keep paragraphs filled

;; Author: David Shepherd <davidshepherd7@gmail.com>
;; Version: 0.0.1
;; Package-Version: 20150703.727
;; Package-Requires: ((dash "2.10.0"))
;; URL: https://github.com/davidshepherd7/aggressive-fill-paragraph-mode
;; Keywords: fill-paragraph, automatic, comments

;;; Commentary:

;; An emacs minor-mode for keeping paragraphs filled in both comments and prose.

;;; Code:

(require 'dash)


;; Functions for testing conditions to suppress fill-paragraph

(defun afp-current-line ()
  (buffer-substring-no-properties (point-at-bol) (point-at-eol)))

(defun afp-markdown-inside-code-block? ()
  """Basic test for indented code blocks in markdown."""
  (and (string-equal major-mode "markdown-mode")
       (string-match-p "^    " (afp-current-line))))


(defun afp-bullet-list-in-comments? ()
  "Try to check if we are inside a bullet pointed list."
  (and (comment-only-p (point-at-bol) (point-at-eol))

       ;; TODO: extend to match any line in paragraph
       (string-match-p (concat "^[ ]*" comment-start "[ ]*[-\\*\\+]")
                       (afp-current-line))))

;; Org mode tables have their own filling behaviour which results in the
;; cursor being moved to the start of the table element, which is no good
;; for us! See issue #6.
(require 'org)
(defun afp-in-org-table? ()
  (interactive)
  (and (derived-mode-p 'org-mode)
       (or (eql (org-element-type (org-element-at-point)) 'table)
           (eql (org-element-type (org-element-at-point)) 'table-row))))

(defcustom afp-suppress-fill-pfunction-list
  (list #'afp-markdown-inside-code-block?
        #'afp-bullet-list-in-comments?
        #'afp-in-org-table?)
  "List of predicate functions of no arguments, if any of these
  functions returns false then paragraphs will not be
  automatically filled."
  :group 'aggressive-fill-paragraph)

(defcustom afp-fill-comments-only-mode-list
  (list 'emacs-lisp-mode 'sh-mode 'python-mode)
  "List of major modes in which only comments should be filled."
  :group 'aggressive-fill-paragraph)



;; The main functions


(defun afp-only-fill-comments (&optional justify)
  "Replacement fill-paragraph function which only fills comments
and leaves everything else alone."
  (fill-comment-paragraph justify)

  ;; returning true says we are done with filling, don't fill anymore
  t)


(defun afp-suppress-fill? ()
  "Check all functions in afp-suppress-fill-pfunction-list"
  (-any? #'funcall afp-suppress-fill-pfunction-list))


(defun afp-ess-fill-comments ()
  "Fill comments in ess-mode (for R and related languages),
taking care with special cases for documentation comments."
  ;; Make sure we have the required libraries (this function is only run
  ;; when (derived-mode-p 'ess-mode) so we should!)
  (require 'ess-mode)
  (require 'ess-roxy)

  (if (ess-roxy-entry-p)
      (ess-roxy-fill-field)
    (afp-only-fill-comments)))


(defun afp-choose-fill-function ()
  "Select which fill paragraph function to use"
  (cond

   ;; In certain modes it is better to use afp-only-fill-comments to avoid
   ;; strange behaviour in code.
   ((apply #'derived-mode-p afp-fill-comments-only-mode-list)
    #'afp-only-fill-comments)

   ;; For python we could also do something with let-binding
   ;; python-fill-paren-function so that code is left alone. This would
   ;; allow docstrings to be filled, but unfortunately filling of strings
   ;; and docstrings are both handled by the same chunk of code, so even
   ;; normal strings would be filled.

   ((derived-mode-p 'ess-mode) #'afp-ess-fill-comments)

   ;; Use the buffer local fill function if it's set
   ((not (null fill-paragraph-function)) fill-paragraph-function)

   ;; Otherwise just use the default one
   (t #'fill-paragraph)))


(defun afp-fill-then-insert-space ()
  "The main function: fill the paragraph (if not suppressed,
using the appropriate fill function), then insert a space."
  (interactive)
  (when (not (afp-suppress-fill?))
    (funcall (afp-choose-fill-function)))
  (insert " "))


(defun afp-insert-space ()
  (interactive)
  (insert " "))



;; Minor mode set up

(define-minor-mode aggressive-fill-paragraph-mode
  nil
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "SPC") #'afp-fill-then-insert-space)
            map))

(defun afp-setup-recommended-hooks ()
  "Install hooks to enable aggressive-fill-paragraph-mode in recommended major modes."
  (interactive)

  (add-hook 'text-mode-hook #'aggressive-fill-paragraph-mode)
  (add-hook 'prog-mode-hook #'aggressive-fill-paragraph-mode))


(provide 'aggressive-fill-paragraph)

;;; aggressive-fill-paragraph.el ends here
