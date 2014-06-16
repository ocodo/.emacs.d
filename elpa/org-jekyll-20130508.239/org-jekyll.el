;;; org-jekyll.el --- Export jekyll-ready posts form org-mode entries
;;;
;;; Author: Juan Reyero
;; Version: 20130508.239
;;; X-Original-Version: 0.4
;;; Keywords: hypermedia
;;; Package-Requires: ((org "8.0"))
;;; Homepage: http://juanreyero.com/open/org-jekyll/
;;; Repository: http://github.com/juanre/org-jekyll
;;; Public clone: git://github.com/juanre/org-jekyll.git
;;;
;;; Commentary:
;;;
;;; Extract subtrees from your org-publish project files that have
;;; a :blog: keyword and an :on: property with a timestamp, and
;;; export them to a subdirectory _posts of your project's publishing
;;; directory in the year-month-day-title.html format that Jekyll
;;; expects.  Properties are passed over as yaml front-matter in the
;;; exported files.  The title of the subtree is the title of the
;;; entry.  The title of the post is a link to the post's page.
;;;
;;; Look at http://orgmode.org/worg/org-tutorials/org-jekyll.html for
;;; more info on how to integrate org-mode with Jekyll, and for the
;;; inspiration of the main function down there.
;;;
;;; Code:

;;(require 'ox-html)

(defvar org-jekyll-category nil
  "Specify a property which, if defined in the entry, is used as
  a category: the post is written to category/_posts. Ignored if
  nil. Use \"lang\" if you want to send posts in different
  languages to different directories.")

(defvar org-jekyll-lang-subdirs nil
  "Make it an assoc list indexed by language if you want to
bypass the category subdir definition and build blog subdirs per
language.")

(defvar org-jekyll-localize-dir nil
  "If non-nil and the lang property is set in the entry,
   org-jekyll will look for a lang.yml file in this directory and
   include it in the front matter of the exported entry.")

(defvar org-jekyll-new-buffers nil
  "Buffers created to visit org-publish project files looking for blog posts.")

(defun org-jekyll-publish-dir (project &optional category)
  "Where does the project go, by default a :blog-publishing-directory
   entry in the org-publish-project-alist."
  (princ category)
  (if org-jekyll-lang-subdirs
      (let ((pdir (plist-get (cdr project) :blog-publishing-directory))
            (langdir (cdr (assoc category org-jekyll-lang-subdirs))))
        (if langdir
            (concat pdir (cdr (assoc category org-jekyll-lang-subdirs))
                    "_posts/")
          (let ((ppdir (plist-get (cdr project) :blog-publishing-directory)))
            (unless ppdir
              (setq ppdir (plist-get (cdr project) :publishing-directory)))
            (concat ppdir
                    (if category (concat category "/") "")
                    "_posts/"))))
    (let ((pdir (plist-get (cdr project) :blog-publishing-directory)))
      (unless pdir
        (setq pdir (plist-get (cdr project) :publishing-directory)))
      (concat pdir
              (if category (concat category "/") "")
              "_posts/"))))

(defun org-jekyll-site-root (project)
  "Site root, like http://yoursite.com, from which blog
  permalinks follow.  Needed to replace entry titles with
  permalinks that RSS agregators and google buzz know how to
  follow.  Looks for a :site-root entry in the org-publish-project-alist."
  (or (plist-get (cdr project) :site-root)
      ""))


(defun org-get-jekyll-file-buffer (file)
  "Get a buffer visiting FILE.  If the buffer needs to be
  created, add it to the list of buffers which might be released
  later.  Copied from org-get-agenda-file-buffer, and modified
  the list that holds buffers to release."
  (let ((buf (org-find-base-buffer-visiting file)))
    (if buf
        buf
      (progn (setq buf (find-file-noselect file))
             (if buf (push buf org-jekyll-new-buffers))
             buf))))

(defun org-jekyll-slurp-yaml (fname)
  (remove "---" (if (file-exists-p fname)
                    (split-string (with-temp-buffer
                                    (insert-file-contents fname)
                                    (buffer-string))
                                  "\n" t))))

(defun ensure-directories-exist (fname)
  (let ((dir (file-name-directory fname)))
    (unless (file-accessible-directory-p dir)
      (make-directory dir t)))
  fname)

(defun org-jekyll-sanitize-string (str project)
  (if (plist-get (cdr project) :jekyll-sanitize-permalinks)
      (progn (setq str (downcase str))
             (dolist (c '(("á" . "a")
                          ("é" . "e")
                          ("í" . "i")
                          ("ó" . "o")
                          ("ú" . "u")
                          ("à" . "a")
                          ("è" . "e")
                          ("ì" . "i")
                          ("ò" . "o")
                          ("ù" . "u")
                          ("ñ" . "n")
                          ("ç" . "s")
                          ("\\$" . "S")
                          ("€" . "E")))
               (setq str (replace-regexp-in-string (car c) (cdr c) str)))
             (replace-regexp-in-string "[^abcdefghijklmnopqrstuvwxyz-]" ""
                                       (replace-regexp-in-string " +" "-" str)))
    str))

(defun org-jekyll-export-entry (project)
  (let* ((props (org-entry-properties nil 'standard))
         (time (cdr (or (assoc "on" props)
                        (assoc "ON" props))))
         (lang (cdr (or (assoc "lang" props)
                        (assoc "LANG" props))))
         (category (if org-jekyll-category
                       (cdr (assoc org-jekyll-category props))
                     nil))
         (yaml-front-matter (copy-alist props)))
    (unless (assoc "layout" yaml-front-matter)
      (push '("layout" . "post") yaml-front-matter))
    (when time
      (let* ((heading (org-get-heading t))
             (title (replace-regexp-in-string "[:=\(\)\?]" ""
                                              (replace-regexp-in-string
                                               "[ \t]" "-" heading)))
             (str-time (and (string-match "\\([[:digit:]\-]+\\) " time)
                            (match-string 1 time)))
             (to-file (format "%s-%s.html" str-time
                              (org-jekyll-sanitize-string title project)))
             (org-buffer (current-buffer))
             (yaml-front-matter (cons (cons "title" heading)
                                      yaml-front-matter))
             html)
        (org-narrow-to-subtree)
        (let ((level (- (org-reduced-level (org-outline-level)) 1))
              (top-level org-html-toplevel-hlevel)
              (contents (buffer-substring (point-min) (point-max)))
              (site-root (org-jekyll-site-root project)))
          ;; Without the promotion the header with which the headline
          ;; is exported depends on the level.  With the promotion it
          ;; fails when the entry is not visible (ie, within a folded
          ;; entry).
          (dotimes (n level nil) (org-promote-subtree))
          (setq html
                (replace-regexp-in-string
                 (format "<h%d id=\"sec-1\">\\(.+\\)</h%d>"
                         top-level top-level)
                 (format
                  "<h%d id=\"sec-1\"><a href=\"%s{{ page.url }}\">\\1</a></h%d>"
                  top-level site-root top-level)
                 (with-current-buffer
                     (org-html-export-as-html nil t t t
                                              '(:tags nil
                                                :table-of-contents nil))
                   (buffer-string))))
          (set-buffer org-buffer)
          (delete-region (point-min) (point-max))
          (insert contents)
          (save-buffer))
        (widen)
        (with-temp-file (ensure-directories-exist
                         (expand-file-name
                          to-file (org-jekyll-publish-dir project category)))
          (when yaml-front-matter
            (insert "---\n")
            (mapc (lambda (pair)
                    (insert (format "%s: %s\n" (car pair) (cdr pair))))
                  yaml-front-matter)
            (if (and org-jekyll-localize-dir lang)
                (mapc (lambda (line)
                        (insert (format "%s\n" line)))
                      (org-jekyll-slurp-yaml (concat org-jekyll-localize-dir
                                                     lang ".yml"))))
            (insert "---\n\n"))
          (insert html))))))

; Evtl. needed to keep compiler happy:
(declare-function org-publish-get-project-from-filename "org-publish"
                  (filename &optional up))

;;;###autoload
(defun org-jekyll-export-current-entry ()
  (interactive)
  (save-excursion
    (let ((project (org-publish-get-project-from-filename buffer-file-name)))
      (org-back-to-heading t)
      (org-jekyll-export-entry project))))

;;;###autoload
(defun org-jekyll-export-blog ()
  "Export all entries in project files that have a :blog: keyword
and an :on: datestamp.  Property drawers are exported as
front-matters, outline entry title is the exported document
title. "
  (interactive)
  (save-excursion
    (setq org-jekyll-new-buffers nil)
    (let ((project (org-publish-get-project-from-filename (buffer-file-name))))
     (mapc
      (lambda (jfile)
        (if (string= (file-name-extension jfile) "org")
            (with-current-buffer (org-get-jekyll-file-buffer jfile)
              ;; It fails for non-visible entries, CONTENT visibility
              ;; mode ensures that all of them are visible.
              (message (concat "org-jekyll: publishing " jfile ))
              (org-content)
              (org-map-entries (lambda () (org-jekyll-export-entry project))
                               "blog|BLOG"))))
      (org-publish-get-base-files project)))
    (org-release-buffers org-jekyll-new-buffers)))

;;;###autoload
(defun org-jekyll-export-project (project-name)
  "Export all entries in project files that have a :blog: keyword
and an :on: datestamp.  Property drawers are exported as
front-matters, outline entry title is the exported document
title. "
  (interactive)
  (save-excursion
    (setq org-jekyll-new-buffers nil)
    (let ((project (assoc project-name org-publish-project-alist)))
     (mapc
      (lambda (jfile)
        (if (string= (file-name-extension jfile) (plist-get (cdr project)
                                                            :base-extension))
            (with-current-buffer (org-get-jekyll-file-buffer jfile)
              ;; It fails for non-visible entries, CONTENT visibility
              ;; mode ensures that all of them are visible.
              (message (concat "org-jekyll: publishing " jfile ))
              (org-content)
              (org-map-entries (lambda () (org-jekyll-export-entry project))
                               "blog|BLOG"))))
      (org-publish-get-base-files project)))
    (org-release-buffers org-jekyll-new-buffers)))

(provide 'org-jekyll)

;;; org-jekyll.el ends here
