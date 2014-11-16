;;; jasmine-coffee --- Helpers for Jasmine (in coffeescript).
;;; Author: Jason Milkins
;;; Version: 20141115
;;; Commentary:
;;
;;  This file is not a part of Emacs
;;
;; ## Installation
;;
;; This script is still very much a work in-progress, so it's not
;; available via MELPA or any other package repository yet.
;;
;; Installation is manual only:
;;
;; Place the `jasmine-coffee.el` script in your Emacs `load-path` and add:
;;
;;     (eval-after-load "coffee-mode"
;;         (require 'jasmine-coffee))
;;
;; To your `~/.emacs` (or ~/.emacs.d/init.el).
;;
;; ## Commands
;;
;; ### Code transforms
;;
;; * `jasmine-coffee/toggle-spec-enabled`
;;
;; Convert an `it` to an `xit`
;;
;; * `jasmine-coffee/var-to-lazy`
;;
;; Convert a variable assignment to `lazy` eg.
;;
;;     myVariable = "value"
;;
;; Becomes
;;
;;     lazy 'myVariable', -> "value"
;;
;; `lazy` is equivalent to RSpec's `let`
;;
;; * `jasmine-coffee/var-to-jlet`
;;
;; Convert a variable assignment to `jlet` eg.
;;
;;     myVariable = "value"
;;
;; Becomes
;;
;;     jlet 'myVariable', -> "value"
;;
;; `jlet` is equivalent to RSpec's `let`
;;
;; * `jasmine-coffee/var-to-jset`
;;
;; Convert a variable assignment to `jset` eg.
;;
;;     myVariable = "value"
;;
;; Becomes
;;
;;     jset 'myVariable', -> "value"
;;
;; `jset` is equivalent to RSpec's `let!`
;;
;; ### Move spec code
;;
;; * `jasmine-coffee/move-to-previous-describe`
;;
;; Move the current selection or line to the previous `describe`
;; statment (moving code from an `it` or `beforeEach` for example.)
;;
;; Note: This command doesn't move code from one `describe` to the
;; previous one.
;;
;; * `jasmine-coffee/move-to-previous-before-each`
;;
;; Move the current selection or line from an `it` spec, to the
;; previous `beforeEach`.
;;
;; ### Verify specs and groups
;;
;; * `jasmine-coffee/verify-describe` & `jasmine-coffee/verify-group`
;;
;; Verify the current describe block via opening a jasmine url, using
;; `jasmine-coffee/base-url` (a custom variable)
;;
;; * `jasmine-coffee/verify-it` & `jasmine-coffee/verify-single`
;;
;; Launch the current spec in a browser window, using the
;; jasmine-coffee/base-url (custom variable)
;;
;; ### Navigations
;;
;; These commands should be self explanatory
;;
;; * `jasmine-coffee/navigate-to-next-it`
;;
;; * `jasmine-coffee/navigate-to-previous-it`
;;
;; * `jasmine-coffee/navigate-to-next-describe`
;;
;; * `jasmine-coffee/navigate-to-previous-describe`
;;
;; * `jasmine-coffee/navigate-to-next-before-each`
;;
;; * `jasmine-coffee/navigate-to-previous-before-each`
;;
;; ### Code / Spec toggling
;;
;; * `jasmine-coffee/toggle-code-and-spec`
;;
;; Switch between the Spec / Code of the current spec or code file.
;; Set the following customizable variables to tailor for your own
;; projects.
;;
;; * `jasmine-coffee/code-root` - location of code, defaults to `app/assets/javascripts`
;; * `jasmine-coffee/spec-root` - location of specs, defaults to `spec/javascripts`
;; * `jasmine-coffee/extension` - standard coffeescript file extension, defaults to `.js.coffee` (Rails style)
;; * `jasmine-coffee/spec-suffix` - standard spec name suffix, defaults to `_spec`
;;
;;; Licence: GPL3
;;
;;; Requires: ((coffee-mode) (s) (dash) (projectile))
;;; Code:

(require 'coffee-mode)
(require 's)
(require 'dash)
(require 'projectile)

(defgroup jasmine-coffee nil
  "Tools for Jasmine on CoffeeScript."
  :group 'coffees)

(defcustom jasmine-coffee/base-url
  "http://localhost:3000/jasmine?spec="
  "Base URL for our Jasmine spec runner."
  :type 'string
  :group 'jasmine-coffee)

(defvar jasmine-coffee/it-regexp
  (rx "it" (any " " "(") (zero-or-more " ")
      (any "'" "\"")
      (group (zero-or-more not-newline))
      (any "'" "\"") (? ")")
      (zero-or-more " ") ",")
  "Regexp to find a jasmine coffee-mode `it'.")

(defvar jasmine-coffee/describe-regexp
  (rx "describe" (any " " "(") (zero-or-more " ")
      (any "'" "\"")
      (group (zero-or-more not-newline))
      (any "'" "\"") (? ")")
      (zero-or-more " ") ",")
  "Regexp to find a jasmine coffee-mode `describe'.")

(defvar jasmine-coffee/before-each-regexp
  (rx "beforeEach" (? "(") (? " ")
      (? "asyncSpec") (? "(") (? " ")  (? "(done)")
      (? " ") "->")
  "Regexp to find a jasmine coffee-mode `beforeEach'.")

(defcustom jasmine-coffee/jasmine-specs-file "JASMINE_SPECS.yml"
  "Name of the Jasmine specs index file."
  :type 'string
  :group 'jasmine-coffee)

(defcustom jasmine-coffee/code-root "/app/assets/javascripts"
  "JavaScript app code root folder."
  :type 'string
  :group 'jasmine-coffee)

(defcustom jasmine-coffee/spec-root "/spec/javascripts"
  "JavaScript app spec root folder."
  :type 'string
  :group 'jasmine-coffee)

(defcustom jasmine-coffee/extension ".js.coffee"
  "Standard extension of coffeescript files in project."
  :type 'string
  :group 'jasmine-coffee)

(defcustom jasmine-coffee/spec-suffix "_spec"
  "Spec filename suffix.
ie. Appears before filename extension."
  :type 'string
  :group 'jasmine-coffee)

(defun jasmine-coffee/kill-line-or-region ()
  "Utility function to kill whole line or region."
  (let (region)
    (setq region (if (use-region-p)
                     (list (region-beginning) (region-end))
                   (list (line-beginning-position) (line-beginning-position 2))))
    (kill-region (car region) (car (cdr region)))))

(defun jasmine-coffee/var-to-function-form (function-call-prefix)
  "Non-interactive convert variable to FUNCTION-CALL-PREFIX form."
  ;; NOTE: This is a very simplistic macro-like implementation, I
  ;; should improve it.
  (beginning-of-line)
  (re-search-forward "=")
  (kill-backward-chars 1)
  (insert ",")
  (beginning-of-line)
  (search-forward-regexp "[^\s]")
  (backward-char)
  (insert (format "%s '" function-call-prefix))
  (search-forward-regexp "[\s]")
  (backward-char)
  (insert "'")
  (delete-horizontal-space)
  (forward-char)
  (delete-horizontal-space)
  (insert " -> "))

(defun jasmine-coffee/move-to-indentation ()
  "Move to the indentation of the current line."
  (move-end-of-line 1)
  (back-to-indentation))

(defun jasmine-coffee/get-region-indent-list ()
  "Collect list of indent columns from region."
  (let (columns-list
        (end (region-end)))
    (save-excursion
      (when (= (point) end) (exchange-point-and-mark))
      (while (< (point) end)
        (jasmine-coffee/move-to-indentation)
        (push (current-column) columns-list)
        (forward-line 1)))
    (reverse columns-list)))

(defun jasmine-coffee/get-current-line-indent-as-list ()
  "Get a list containing the current line's indentation column."
  (jasmine-coffee/move-to-indentation)
  (list (current-column)))

(defun jasmine-coffee/get-indent-list ()
  "Get a list of indent positions from either the region or the current line."
  (if (use-region-p)
      (jasmine-coffee/get-region-indent-list)
    (jasmine-coffee/get-current-line-indent-as-list)))

(defun jasmine-coffee/reset-indentation (indent-list)
  "From the current posion reset indentation using the supplied INDENT-LIST."
  (let* ((current (current-column))
         (first (pop indent-list))
         (difference (- current first)))
    (while indent-list
      (forward-line)
      (indent-line-to (+ difference (pop indent-list))))))

(defun jasmine-coffee/move-to-previous-thing (pattern)
  "Move the current line or region to the previous thing defined by PATTERN."
  (save-excursion
    (let* ((indent-list (jasmine-coffee/get-indent-list)))
      (jasmine-coffee/kill-line-or-region)
      (re-search-backward pattern)
      (end-of-line)
      (save-excursion
        (yank))
      (delete-horizontal-space)
      (coffee-newline-and-indent)
      (jasmine-coffee/reset-indentation indent-list))))

(defun jasmine-coffee/spec-meta-data (pattern)
  "Collec jasmine spec metadata matching nearest PATTERN."
  (let* ((start-column 0)
         (spec-string "")
         spec-column
         spec-line)
    (save-excursion
      (move-end-of-line 1)
      (re-search-backward pattern)
      (setq start-column (current-column))
      (setq spec-column start-column)
      (setq spec-line  (line-number-at-pos))
      (setq spec-string (match-string-no-properties 1))
      (while
          (re-search-backward jasmine-coffee/describe-regexp 0 t)
        (when (< (current-column) start-column)
          (setq start-column (current-column))
          (setq spec-string (format "%s %s" (match-string 1) spec-string)))))
    (list
     (url-encode-url (replace-regexp-in-string "#" "%23" spec-string))
     spec-column
     spec-line
     (buffer-file-name))))

(defun jasmine-coffee/verify-thing (pattern)
  "Launch a composed jasmine spec URL.
For the nearest thing behind the
current cursor, defined by PATTERN."
  (let* ((spec-string (car (jasmine-coffee/spec-meta-data pattern))))
    (save-current-buffer)
    (browse-url (concat jasmine-coffee/base-url spec-string))))

(defun jasmine-coffee/var-to-lazy ()
  "Convert local var on the current line to a `lazy'.
See Jasmine-let github.com:xaethos/jasmine-let.git"
  (interactive)
  (save-excursion
    (jasmine-coffee/var-to-function-form "lazy")))

(defun jasmine-coffee/var-to-jlet ()
  "Convert local var on the current line to a `jlet'.

`jlet' is a lazy evaluation variable form for jasmine, similar to
rspec's `let'.  It is not a part of jasmine."
  (interactive)
  (save-excursion
    (jasmine-coffee/var-to-function-form "jlet")))

(defun jasmine-coffee/var-to-jset ()
  "Convert local var on the current line to a `jset'.

`jset' is a variable evaluation form similar to rspec's `let!'.
It is not a part of jasmine."
  (interactive)
  (save-excursion
    (jasmine-coffee/var-to-function-form "jset")))

(defun jasmine-coffee/move-to-previous-describe ()
  "Move the current line or region to the previous `describe' body."
  (interactive)
  (jasmine-coffee/move-to-previous-thing jasmine-coffee/describe-regexp))

(defun jasmine-coffee/move-to-previous-before-each ()
  "Move the current line or region to the previous `beforeEach' body."
  (interactive)
  (jasmine-coffee/move-to-previous-thing jasmine-coffee/before-each-regexp))

(defun jasmine-coffee/verify-describe ()
  "Compose and launch Spec URL for the current `describe' block."
  (interactive)
  (jasmine-coffee/verify-thing jasmine-coffee/describe-regexp))

(defun jasmine-coffee/verify-group ()
  "Alias for verify describe.
Compose and launch Spec URL for the current `describe' block."
  (interactive)
  (jasmine-coffee/verify-describe))

(defun jasmine-coffee/verify-it ()
  "Compose and launch spec URL for the current `it' spec."
  (interactive)
  (jasmine-coffee/verify-thing jasmine-coffee/it-regexp))

(defun jasmine-coffee/verify-single ()
  "Alias for verify-it.
Compose and launch spec URL for the current `it' spec."
  (interactive)
  (jasmine-coffee/verify-it))

(defun jasmine-coffee/navigate-to-next-thing (regexp)
  "Navigate cursor to the body of the next matching REGEXP."
  (with-demoted-errors
      (re-search-forward regexp))
  (forward-line 1)
  (jc/move-to-indentation))

(defun jasmine-coffee/navigate-to-previous-thing (regexp)
  "Navigate cursor to the body of the previous REGEXP."
  (with-demoted-errors
      (re-search-backward regexp))
  (forward-line 1)
  (jc/move-to-indentation))

(defun jasmine-coffee/navigate-to-next-it ()
  "Navigate cursor to the body of the next `it' spec."
  (interactive)
  (jasmine-coffee/navigate-to-next-thing jasmine-coffee/it-regexp))

(defun jasmine-coffee/navigate-to-previous-it ()
  "Navigate cursor to the body of the previous `it' spec."
  (interactive)
  (jasmine-coffee/navigate-to-previous-thing jasmine-coffee/it-regexp))

(defun jasmine-coffee/navigate-to-next-describe ()
  "Navigate cursor to the body of the next `describe' block."
  (interactive)
  (jasmine-coffee/navigate-to-next-thing jasmine-coffee/describe-regexp))

(defun jasmine-coffee/navigate-to-previous-describe ()
  "Navigate cursor to the body of the previous `describe' block."
  (interactive)
  (jasmine-coffee/navigate-to-previous-thing jasmine-coffee/describe-regexp))

(defun jasmine-coffee/navigate-to-next-before-each ()
  "Navigate cursor to the body of the next `beforeEach' block."
  (interactive)
  (jasmine-coffee/navigate-to-next-thing jasmine-coffee/before-each-regexp))

(defun jasmine-coffee/navigate-to-previous-before-each ()
  "Navigate cursor to the body of the previous `beforeEach' block."
  (interactive)
  (jasmine-coffee/navigate-to-previous-thing jasmine-coffee/before-each-regexp))

(defun jasmine-coffee/toggle-code-and-spec ()
  "Toggle between the current spec and code."
  (interactive)
  (let* ((ext-spec    (format "%s%s" jasmine-coffee/spec-suffix jasmine-coffee/extension))
         (ext-code    jasmine-coffee/extension)
         (is-coffee   (s-contains? jasmine-coffee/extension (buffer-file-name)))
         (is-a-spec   (s-contains? ext-spec (buffer-file-name)))
         (roots       (list jasmine-coffee/code-root jasmine-coffee/spec-root))
         (exts        (list ext-code ext-spec))
         target-file)

    (if is-coffee
        (progn
          (when is-a-spec
            (setq roots (nreverse roots))
            (setq exts (nreverse exts)))

          (add-to-list 'roots (buffer-file-name) t)
          (add-to-list 'exts (apply 's-replace roots) t)

          (find-file (apply 's-replace exts))

          (message "Not a jasmine coffee file")))))

(defun jasmine-coffee/jump-to-data-store ()
  "Jump to a data-store."

  )

(defun jasmine-coffee/toggle-view-model-and-view ()
  "Jump between view and view model."
  (let (
        (jump-list
         '(templates ("/templates/" "/templates/\\(.*\\)\\(\\.hamlc\\)" "/templates/\\1.hamlc")
                     views ("/views/" "/views/\\(.*\\)\\_view\\(\\.js.coffee\\)" "/views/\\1_view\\2")
                     view-models ("/view_models/" "/view_models/\\(.*\\)\\(\\.js.coffee\\)" "/view_models/\\1\\2"))))))

(defun jasmine-coffee/find-spec-by-url (&optional url)
  "Find a jasmine spec by the supplied spec URL.

It uses the JASMINE_SPECS file to find and open the
spec described by the given URL."

  )

(defun jasmine-coffee/collect-specs (pattern)
  "Gather the spec strings by PATTERN and TYPE, then add them to the SPEC-LIST."
  (let (current-spec spec-list)
    (goto-char (point-max))
    (while (re-search-backward pattern 0 t)
      (jc/end-of-line)
      (setq current-spec
            (apply 'format
                   (-flatten
                    (list "- file: %s\n  line: %d\n  col: %d\n  url: %s"
                          (reverse (jasmine-coffee/spec-meta-data pattern))))))
      (add-to-list 'spec-list current-spec t)
      (beginning-of-line))
    spec-list))

(defun jasmine-coffee/index-specs (&optional spec-file)
  "Index a jasmine-spec SPEC-FILE."

  (when spec-file (find-file spec-file))

  (let* ((ext-spec      (format "%s%s" jasmine-coffee/spec-suffix jasmine-coffee/extension))
         (is-a-spec     (s-contains? ext-spec (buffer-file-name)))
         (path-filename (buffer-file-name))
         spec-line
         spec-column
         current-spec)
    (if is-a-spec
        (save-excursion
          (-flatten (list
                     (jasmine-coffee/collect-specs jasmine-coffee/describe-regexp)
                     (jasmine-coffee/collect-specs jasmine-coffee/it-regexp))))
      nil)))

(defun jasmine-coffee/index-spec-to-file (&optional spec-file index-file)
  "Write SPEC-FILE index to INDEX-FILE."
  (let* (specs-list)
    (unless spec-file (setq spec-file (buffer-file-name)))
    (unless (and index-file
                 (file-exists-p index-file))
      (setq index-file
            (format "%s/%s"
                    projectile--project-root
                    jasmine-coffee/jasmine-specs-file)))
    (setq specs-list (jasmine-coffee/index-specs spec-file))
    (when specs-list
      (append-to-file (s-join "\n" specs-list) nil index-file))))

(defun jasmine-coffee/index-all-specs (&optional index-file spec-folder)
  "Generate jasmine specs INDEX-FILE for all specs found in SPEC-FOLDER."
  (unless (and index-file
               (file-exists-p index-file))
    (setq index-file
          (format "%s/%s"
                  projectile--project-root
                  jasmine-coffee/jasmine-specs-file)))

  (unless spec-folder
    (setq spec-folder
          (format "%s/%s"
                  projectile--project-root
                  jasmine-coffee/spec-root)))

  (dolist (f (projectile-files-in-project-directory spec-folder))
    (jasmine-coffee/index-spec-to-file f index-file)))

(defun jasmine-coffee/toggle-spec-enabled ()
  "Toggle the current `it' spec on/off."
  (interactive)
  (save-excursion
    (jc/end-of-line)
    (with-demoted-errors
        (re-search-backward (rx line-start (+ blank) "it" (group (? "x")))))
    (jc/move-to-indentation)
    (when (looking-at "it ") (forward-char 2) (insert "x"))
    (when (looking-at "itx") (forward-char 2) (delete-char 1))))

(defun jc/move-to-indentation ()
  "Internal function to jump to indentation column."
  (jc/end-of-line)
  (back-to-indentation))

(defun jc/end-of-line ()
  "Internal function jump to end of line."
  (move-end-of-line 1))

(provide 'jasmine-coffee)
;;; jasmine-coffee.el ends here
