;;; show-css.el --- Show the css of the html attribute the cursor is on
;;
;; Copyright (C) 2012 Sheldon McGrandle
;;
;; Author: Sheldon McGrandle <developer@rednemesis.com>
;; Version: 1.01
;; Created: 1st February 2013
;; Keywords: hypermedia
;; URL: https://github.com/smmcg/showcss-mode
;; Package-Requires: ((doom "1.3") (s "1.10.0"))
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; author of this program or to the Free Software Foundation, 675 Mass
;; Ave, Cambridge, MA 02139, USA.

;;; Commentary:
;;
;; DESCRIPTION AND USAGE
;;
;; Show CSS is a minor mode for emacs.
;;
;; With showcss-mode turned on, as you navigate around an HTML file the
;; matching css for that element will be displayed in another buffer.
;;
;; In the current html buffer, if you move the cursor over a class=".*?"
;; or id=".*?" a buffer will open with the external css file loaded and
;; scrolled to the matching selector.
;;
;; Show Css will look at the <link> tags and a custom comment tag to get
;; the location of external css files.
;;
;; Show Css looks for a comment with this regex:
;; <!-- showcss: \\(.*?\\) -->
;;
;; For example:
;; <!-- showcss: /home/user/projects/facebook/site/css/main.css -->
;; or
;; <!-- showcss: ./sass_files/main.sass-->
;;
;; The comment is useful if you want to use sass files directly instead
;; of compiling them.  Also showcss-mode will only use local files.  So
;; if you use css on a remote server, you will need to use the showcss
;; tag in you html file and have it point to a local copy of that css.
;;
;; INSTALLATION
;;
;; Put this in your init.el or .emacs file:
;;
;;   (autoload 'showcss-mode "show-css"
;;      "Display the css of the class or id the cursor is at" t)
;;
;; Personally, I find this mode to distracting to use all the time, so I
;; use this function to quickly toggle the mode on and off.
;;
;;   (defun sm/toggle-showcss()
;;     "Toggle showcss-mode"
;;     (interactive)
;;     (if (derived-mode-p
;;          'html-mode
;;          'nxml-mode
;;          'nxhtml-mode
;;          'web-mode
;;          'handlebars-mode)
;;         (showcss-mode 'toggle)
;;       (message "Not in an html mode")))
;;   (global-set-key (kbd "C-c C-k") 'sm/toggle-showcss)
;;
;; BUGS
;;
;; Please report any bugs found to the github repository
;; https://github.com/smmcg/showcss-mode
;;
;; Also, it you have ideas, suggestions, or advice; please
;; let me know.  Use the github issues tool or send me
;; an email.
;;

;;; Code:


(require 'buffer-combine)
(require 'doom)
(require 's)


(defgroup showcss nil
  "Customize showcss"
  :prefix "showcss/"
  :group 'Text)

(defcustom showcss/update-delay 0.5
  "Number of seconds of idle time from last keypress
before updating selectors display"
  :group 'showcss
  :type 'number)

(defcustom showcss/use-html-tags t
  "Use the <link ...> tag in addition to the <!-- --> comments.
Turn off if you want to only comments to explicitly set the css
to view"
  :group 'showcss
  :type 'boolean)

(defcustom showcss/display-buffer-mode "css-mode"
  "The mode that the display buffer is set to.  It defaults to
css-mode, but it could be any other mode"
  :group 'showcss
  :type 'string)

(defcustom showcss/hide-source-buffers nil
  "Add a space to the front of the buffer name of all
the source files so they don't clutter up the file list"
  :group 'showcss
  :type 'boolean)

;; add custom:
;;   turn of automatic scanning
;;   set mode display buffer is in
;;   don't display file name in header
;;   don't display file path in header
;;   don't display buttons in header

(defgroup showcss-faces nil
  "Customize showcss faces"
  :prefix "showcss/"
  :group 'showcss)

(defface showcss/region-face
  '((t (:background "grey50")))
  "Highlight the full selector"
  :group 'showcss-faces)

(defface showcss/source-region-face
  '((t (:foreground "grey60" :background "grey40")))
  "Highlight the full selector in the source css buffer"
  :group 'showcss-faces)

(defface showcss/breadcrumb-id-and-class-face
  '((t (:foreground "black" :background "white")))
  "Highlight the breadcrumb tags"
  :group 'showcss-faces)

(defface showcss/breadcrumb-tag-face
  '((t (:foreground "black" :background "white")))
  "Highlight the breadcrumb tags"
  :group 'showcss-faces)

(defface showcss/breadcrumb-seperator-face
  '((t (:foreground "grey50")))
  "Highlight the breadcrumb seperater"
  :group 'showcss-faces)

(defface showcss/header-face
  '((t (:background "grey50")))
  "Face for headers"
  :group 'showcss-faces)

(defface showcss/header-filepath-face
  '((t (:foreground "grey20")))
  "Face for headers"
  :group 'showcss-faces)


(defvar showcss/css-buffer nil
  "The buffer that contains the css file")
(make-local-variable 'showcss/css-buffer)

(defvar showcss/html-buffer nil
  "The buffer that contains the html file")
(make-local-variable 'showcss/html-buffer)

(defvar showcss/parents nil
  "The list of parents for the current tag")
(make-local-variable 'showcss/parents)

;; keep global
(defvar showcss/timer nil)

(defvar showcss/display-buffer nil
  "The buffer to display the matches in")
(make-variable-buffer-local 'showcss/display-buffer)


(defun showcss/set-css-buffer()
  "showcss will look for css files in the following places:
1.  Look for the <!-- showcss: ... --> in the html file
2.  Look at the css declarations in the html <head>

Showcss will only use local files.  So if you use css on a remote
server, you will need to use the showcss tag in you html file and
have it point to a local copy of that css.

Find the name of the css file using this regex:
<!-- showcss: \\(.*?\\) -->
Eg:
<!-- showcss: /home/sm/projects/some project/site/css/main.css -->"

  (setq showcss/css-buffer nil)

  (let ((csslist nil))

    (when showcss/use-html-tags
      (save-excursion
        ;; get the <link> css
        (goto-char (point-min))
        (while (re-search-forward "<link\\(.\\|\n\\)*?>" nil t)
          (let ((tag-start (match-beginning 0))
                (tag-end (match-end 0)))
            (goto-char tag-start)
            (when (re-search-forward
                   "\\(type=\"text/css\"\\|rel=\"stylesheet\"\\)" tag-end t)
              (goto-char tag-start)
              (when (re-search-forward "href=\"\\([^:]*?\\)\"" tag-end t)
                (let ((css-file (file-truename (substring-no-properties
                                                (match-string 1)))))
                  (when (file-exists-p css-file)
                    (setq csslist (cons css-file csslist))))))
            (goto-char tag-end)))

        ;; get @imports
        ;; todo: make sure this is in a <style> tag and not
        ;; part of some example code
        (goto-char (point-min))
        (while (re-search-forward "@import .*?\"\\(.*?\\)\"" nil t)
          (setq csslist (cons (substring-no-properties
                               (match-string 1)) csslist)))))

    ;; get the <!-- showcss ... --> comment if any
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "<!-- showcss: \\(.*?\\) -->" nil t)
        (when (file-exists-p (match-string 1))
          (setq csslist
                (cons (substring-no-properties (match-string 1))
                      csslist)))))

    ;; load the css files into buffers
    (mapc (lambda (css-file)
            (setq showcss/css-buffer
                  (cons (find-file-noselect css-file)
                        showcss/css-buffer)))
          csslist)))


(defun showcss/parse-html()
  "Parse the html file with doom.el"
  (setq showcss/parents nil)
  (save-excursion
    (re-search-backward "\\(<\\|>\\)" nil t)
    (forward-char)
    ;; if not looking at a / or ! (comment or closing tag)
    (unless (or (looking-at "\\(/\\|!\\)")
                (looking-back ">"))
      (backward-char)
      (let ((bookmark-tag "<showcss />"))
        (insert bookmark-tag)  ;TODO: this needs to not affect undo

        (let ((doc nil)
              (node nil)
              (not-tag t))

          (condition-case nil
              (progn
                (setq doc (doom-make-document-from-xml
                           (libxml-parse-html-region (point-min) (point-max))))
                (setq node (car (doom-document-get-elements-by-tag-name
                                 doc 'showcss))))
            (error (message "Malformed document")))

          (search-backward bookmark-tag)
          (delete-char (length bookmark-tag))
          ;; Find the next sibling tag from the inserted
          ;; <showcss-... /> bookmark-tag, since that is the
          ;; one we really want, skipping text tags.
          (while not-tag
            (condition-case nil
                (setq node (doom-node-next-sibling node))
              (error nil))
            (when (= (doom-node-type node) 1)
              (setq not-tag nil)
              (showcss/up-walk node)
              (showcss/find-selectors)))))
      t)))


(defun showcss/up-walk(node)
  "Walk up the parse tree and collect information about each tag"
  (when (doom-node-p node)
    (let* ((node-name (doom-node-name node))
           (node-list (list node-name))
           (attr-list '()))

      (when (doom-node-has-attributes node)
        (dolist (attr (doom-node-attributes node))
          (let ((attr-name (doom-node-name attr))
                (attr-val (doom-node-value attr)))
            (if (string= attr-name "class")
                (setq node-list
                      (cons (cons attr-name
                                  (s-split " " attr-val t))
                            node-list))
              (setq node-list (cons (list attr-name attr-val) node-list))))))

      (setq showcss/parents (cons (reverse node-list) showcss/parents))

      (showcss/up-walk (doom-node-parent-node node)))))


(defun showcss/find-selectors ()
  "For each source buffer, get a list
of positions of matched selectors"
  (let ((all-elements nil)
        (html-buffer (current-buffer)))
    ;; for each buffer
    (dolist (source-buffer showcss/css-buffer)
      (let ((buffer-and-fragments (list source-buffer)))
        ;; for each tag
        (dolist (elements (reverse showcss/parents))
          (let ((tag-name (car elements))
                (tag-id nil)
                (tag-class nil)
                (data nil))

            ;; make sure each class and id is associated
            ;; with the right list
            (dolist (attribs (cdr elements))
              (cond ((string= (car attribs) "id")
                     (setq tag-id (nth 1 attribs)))
                    ((string= (car attribs) "class")
                     (setq tag-class (cdr attribs)))))

            (when tag-name
              (setq buffer-and-fragments
                    (append (showcss/get-points source-buffer 'tag tag-name)
                            buffer-and-fragments)))
            (when tag-class
              (setq buffer-and-fragments
                    (append (showcss/get-points source-buffer 'class tag-class)
                            buffer-and-fragments)))
            (when tag-id
              (setq buffer-and-fragments
                    (append (showcss/get-points source-buffer 'id tag-id)
                            buffer-and-fragments)))))
        (setq all-elements
              (cons (reverse (delete-dups buffer-and-fragments))
                    all-elements))))

    (showcss/display-info all-elements html-buffer)))


(defun showcss/get-points(source-buffer type value)
  "Parse css and return a list of beginning and end for each
matching selector (and its declarations)"
  (let ((search-string (showcss/build-selector type value))
        (locations ())
        (start-point nil)
        (end-point nil)
        ;; so this can be passed on to showcss/test-selector:
        ;; (origin-tag (car (reverse showcss/parents)))
        (parents showcss/parents))
    (save-excursion
      (with-current-buffer source-buffer
        (goto-char (point-min))
        (while (showcss/search search-string)
          (if (showcss/search "}" 'backwards)
              (progn
                (when  (looking-at "}")  (forward-char))  ; forwards over }
                (while (looking-at "\n") (forward-char))) ; and skip over blank lines
            (goto-char (point-min)))    ; if on first line of file
          (setq start-point (point))    ; set point
          (showcss/search "{")
          (showcss/search "}")
          (when (looking-at "\n")
            (forward-char))
          (setq end-point (point))
          (setq locations (cons (list start-point end-point) locations)))))
    locations))


(defun showcss/search(regexp &optional backwards)
  "Search forward or backward for first
regexp not inside a comment or string."
  (let ((start (point)))
    (if backwards
        (while (and (re-search-backward regexp nil t 1)
                    (nth 8 (syntax-ppss))))
      (while (and (re-search-forward regexp nil t 1)
                  (nth 8 (syntax-ppss)))))
    (not (= start (point)))))


(defun showcss/build-selector (type value)
  "Convert a class to \".class\" or an id to \"#id\"
and create a regex to be used for searching in the css files.
eg: \"\\\\(\\\\.some_class\\\\)[ ,\\n{]\""
  (let ((full-selector nil))
    (cond ((eq type 'class)
           (setq full-selector (concat "\\(\\." (s-join "\\>\\|\\." value)
                                       "\\)[^-_[:alpha:]]")))
          ((eq type 'id)
           (setq full-selector (format "#%s\\>[^-_[:alpha:]]" value)))
          ((eq type 'tag)
           (setq full-selector (format "\\b%s\\b" value)))
          (t
           (error (format "Wrong type of selector: %s" type))))
    full-selector))


(defun showcss/build-breadcrumb(parents)
  "Build a breadcrumb that looks like this:
html > body > div#id.class > div.class.class > ul > li"
  (let* ((breadcrumb nil)
         (separator-string " > ")
         (separator (propertize
                     separator-string
                     'face 'showcss/breadcrumb-seperator-face)))
    (dolist (full-tag parents)
      (let ((tag-name
             (if (symbolp (car full-tag))
                 (symbol-name (car full-tag))
               (car full-tag)))
            (tag-class nil)
            (tag-id nil)
            (section nil))
        (dolist (attribs (cdr full-tag))
          (cond ((string= (car attribs) "id")
                 (setq tag-id
                       (propertize (concat "#" (nth 1 attribs))
                                   'face 'showcss/breadcrumb-id-and-class-face)))
                ((string= (car attribs) "class")
                 (setq tag-class
                       (propertize (concat "." (s-join "." (cdr attribs)))
                                   'face 'showcss/breadcrumb-id-and-class-face)))))
        ;; (setq section (nconc (list tag-name tag-id) tag-class))
        (setq tag-name (propertize tag-name 'face 'showcss/breadcrumb-tag-face))
        (setq breadcrumb
              (cons (concat tag-name tag-id tag-class)
                    breadcrumb))
        ;; (concat tag-name tag-id tag-class)
        ;; (message full-tag)
        ))
    (s-join separator breadcrumb)))


(defun showcss/display-info(data html-buffer)
  "Create a display buffer and send the data to it."
  (let ((display-buffer (get-buffer-create "Show CSS"))
        (parents showcss/parents))
    (set-buffer display-buffer)
    (switch-to-buffer-other-window display-buffer)
    ;; (css-mode) ; should this be called each time?
    (bc/start data parents :hidden showcss/hide-source-buffers)
    (switch-to-buffer-other-window html-buffer)))


(defun showcss/main()
  (interactive)
  (unless (showcss/parse-html)
    (bc/start nil nil)))


(defun showcss/timerfunc()
  (when (and showcss/html-buffer
             (string= (buffer-name) (buffer-name showcss/html-buffer))
             (memq last-command '(next-line
                                  previous-line
                                  right-char
                                  left-char
                                  forward-word
                                  backward-word
                                  forward-sexp
                                  backward-sexp)))
    (showcss/main)))


;;;###autoload
(define-minor-mode showcss-mode
  "Display the css of the class or id the cursor is at.
Visit https://github.com/smmcg/showcss-mode to view the
git repository"

  :init-value nil
  :lighter " ShowCSS"
  :keymap '(([C-c C-u] . showcss/parse-html))

  (cond (showcss-mode
         (showcss/set-css-buffer)
         (setq showcss/html-buffer (current-buffer))
         (unless showcss/timer
           (setq showcss/timer
                 (run-with-idle-timer showcss/update-delay
                                      t 'showcss/timerfunc)))
         ;; (add-hook 'after-save-hook 'showcss/parse-html nil t)
         )
        (t
         (setq showcss/html-buffer nil)
         (bc/remove-source-overlays)
         ;; TODO: cancel-timer only after all showcss-modes have been turned off.
         ;; (cancel-timer showcss/timer)
         )))


(provide 'show-css)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; show-css.el ends here
