;;; ego-template.el --- templating system based on mustache, required by ego

;; Copyright (C)  2015 Feng Shu, Kuangdash
;;                2012, 2013, 2014, 2015 Kelvin Hu

;; Author: Kelvin Hu <ini DOT kelvin AT gmail DOT com>
;;         Feng Shu  <tumashu AT 163.com>
;;         Kuangdash <kuangdash AT 163.com>
;; Keywords: org-mode, convenience, beautify
;; Homepage: https://github.com/emacs-china/EGO

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

;; templating system based on mustache.el, to replace `format-spec'.

;;; Code:

(require 'ox)
;; (require 'mustache)
(autoload 'mustache-render "mustache")
(require 'ego-util)
(require 'ego-config)
(require 'ego-git)


(defun ego--get-template-file (template-file-name)
  "Get path of template file which name is `template-file-name'."
  (car (remove nil (mapcar
                    #'(lambda (dir)
                        (let ((file (concat (file-name-as-directory dir)
                                            template-file-name)))
                          (when (file-exists-p file)
                            file)))
                    (ego--get-theme-dirs nil nil 'templates)))))

(defun ego--get-title ()
  "Get the title of org file."
  (or (ego--read-org-option "TITLE")
      (file-name-sans-extension (buffer-name))))

(defun ego--get-category (org-file)
  "Get org file category presented by ORG-FILE, return all categories if
ORG-FILE is nil. "
  (let ((func (ego--get-config-option :retrieve-category-function)))
    (if (functionp func)
        (funcall func org-file)
      (funcall 'ego--get-file-category org-file))))

(defun ego--get-cache-item (key)
  "Get the item associated with KEY in `ego--item-cache', if `ego--item-cache' is
nil or there is no item associated with KEY in it, return nil."
  (and ego--item-cache
       (plist-get ego--item-cache key)))

(defun ego--update-cache-item (key value)
  "Update the item associated with KEY in `ego--item-cache', if `ego--item-cache' is
nil, initialize it."
  (if ego--item-cache
      (plist-put ego--item-cache key value)
    (setq ego--item-cache `(,key ,value)))
  value)

(defmacro ego--get-cache-create (key &rest body)
  "Firstly get item from `ego--item-cache' with KEY, if item not found, evaluate
BODY and push the result into cache and return it."
  `(or (ego--get-cache-item ,key)
       (ego--update-cache-item ,key (funcall (lambda () ,@body)))))

(defun ego--render-header (&optional param-table)
  "Render the header on each page. PARAM-TABLE is the hash table from mustache
to render the template. If it is not set or nil, this function will try to build
a hash table accordint to current buffer."
  (mustache-render
   (ego--get-cache-create
    :header-template
    (message "EGO: Read header.mustache from file")
    (ego--file-to-string (ego--get-template-file "header.mustache")))
   (or param-table
       (ht ("page-title" (concat (funcall (ego--get-config-option :get-title-function))
                                 " - " (ego--get-config-option :site-main-title)))
           ("author" (or (ego--read-org-option "AUTHOR")
                         user-full-name "Unknown Author"))
           ("description" (ego--read-org-option "DESCRIPTION"))
           ("keywords" (ego--read-org-option "TAGS"))))))

(defun ego--render-navigation-bar (&optional param-table)
  "Render the navigation bar on each page. it will be read firstly from
`ego--item-cache', if there is no cached content, it will be rendered
and pushed into cache from template. PARAM-TABLE is the hash table for mustache
to render the template. If it is not set or nil, this function will try to
render from a default hash table."
  (let ((site-domain (ego--get-site-domain))
        (cat-real nil))
    (ego--get-cache-create
     :nav-bar-html
     (message "EGO: Render navigation bar from template")
     (mustache-render
      (ego--get-cache-create
       :nav-bar-template
       (message "EGO: Read nav.mustache from file")
       (ego--file-to-string (ego--get-template-file "nav.mustache")))
      (or param-table
          (ht ("site-main-title" (ego--get-config-option :site-main-title))
              ("site-sub-title" (ego--get-config-option :site-sub-title))
              ("nav-categories"
               (mapcar
                #'(lambda (cat)
                    (ht ("category-uri"
                         (concat "/" (ego--encode-string-to-url cat) "/"))
                        ("category-name" (capitalize cat))))
                (setq ego--category-show-list
                      (sort (cl-remove-if
                             #'(lambda (cat)
                                 (or (string= cat "index")
                                     (string= cat "about")
                                     (not (plist-get (cdr (or (assoc cat
                                                                     ego--category-config-alist)
                                                              (ego--get-category-setting (ego--get-config-option :default-category))))
                                                     :category-index))))
                             (ego--get-category nil))
                            'string-lessp))))
              ("nav-summary"
               (mapcar
                #'(lambda (cat)
                    (if (equal cat (caar (-filter #'(lambda (element) (equal :tags (cadr element)))
                                                     (ego--get-config-option :summary))))
                        (setq cat-real "tags")
                      (setq cat-real cat))
                    (ht ("summary-item-uri"
                         (concat "/" (ego--encode-string-to-url cat-real) "/"))
                        ("summary-item-name" (capitalize cat))))
                (mapcar #'car (ego--get-config-option :summary))))
              ("nav-source-browse"
               (let ((list (ego--get-config-option :source-browse-url)))
                 (when list
                   (ht ("source-browse-name" (car list))
                       ("source-browse-uri" (car (cdr list)))))))
              ("nav-about"
               (let ((list (ego--get-config-option :about)))
                 (when list
                   (ht ("about-name" (car list))
                       ("about-uri" (car (cdr list)))))))
              ("nav-rss"
               (let ((list (ego--get-config-option :rss)))
                 (when list
                   (ht ("rss-name" (car list))
                       ("rss-uri" (car (cdr list)))))))
              ("avatar" (ego--get-config-option :personal-avatar))
              ("site-domain" (if (string-match
                                  "\\`https?://\\(.*[a-zA-Z]\\)/?\\'"
                                  site-domain)
                                 (match-string 1 site-domain)
                               site-domain))))))))

(defun ego--render-content (&optional template param-table)
  "Render the content on each page. TEMPLATE is the template name for rendering,
if it is not set of nil, will use default post.mustache instead. PARAM-TABLE is
similar to `ego--render-header'."
  (mustache-render
   (ego--get-cache-create
    (if template
        (intern (replace-regexp-in-string "\\.mustache$" "-template" template))
      :post-template)
    (message (concat "Read " (or template "post.mustache") " from file"))
    (ego--file-to-string (ego--get-template-file
                         (or template "post.mustache"))))
   (or param-table
       (ht ("title" (funcall (ego--get-config-option :get-title-function)))
           ("content" (cl-flet ((org-html-fontify-code
                                 (code lang)
                                 (when code (org-html-encode-plain-text code))))
                        (let ((org-export-function (ego--get-config-option :org-export-function)))
                          (when (functionp org-export-function)
                            (funcall org-export-function)))))))))

(defun ego--default-org-export ()
  "A function with can export org file to html."
  (org-export-as 'html nil nil t nil))

(defun ego--render-footer (&optional param-table)
  "Render the footer on each page. PARAM-TABLE is similar to
`ego--render-header'."
  (mustache-render
   (ego--get-cache-create
    :footer-template
    (message "EGO: Read footer.mustache from file")
    (ego--file-to-string (ego--get-template-file "footer.mustache")))
   (or param-table
       (let* ((filename (buffer-file-name))
              (title (funcall (ego--get-config-option :get-title-function)))
              (default-category (ego--get-config-option :default-category))
              (date (ego--fix-timestamp-string
                     (or (ego--read-org-option "DATE")
                         (format-time-string "%Y-%m-%d"))))
              (tags (ego--read-org-option "TAGS"))
              (tags (if tags
                        (mapcar
                         #'(lambda (tag-name)
                             (ht ("link" (ego--generate-summary-uri "tags" tag-name))
                                 ("name" tag-name)))
                         (delete "" (mapcar 'string-trim (split-string tags "[:,]+" t))))))
              (category (ego--get-category filename))
              (config (cdr (or (assoc category ego--category-config-alist)
                               (ego--get-category-setting default-category))))
              (uri (funcall (plist-get config :uri-generator)
                            (plist-get config :uri-template) date title)))
         (ht ("show-meta" (plist-get config :show-meta))
             ("show-comment" (plist-get config :show-comment))
             ("date" date)
             ("mod-date" (if (not filename)
                             (format-time-string "%Y-%m-%d")
                           (or (ego--git-last-change-date
                                (ego--get-repository-directory)
                                filename)
                               (format-time-string
                                "%Y-%m-%d"
                                (nth 5 (file-attributes filename))))))
             ("tags" tags)
             ("tag-links" (if (not tags) "N/A"
                            (mapconcat
                             #'(lambda (tag)
                                 (mustache-render
                                  "<a href=\"{{link}}\">{{name}}</a>" tag))
                             tags " : ")))
             ("author" (or (ego--read-org-option "AUTHOR")
                           user-full-name
                           "Unknown Author"))
             ("disqus-id" uri)
             ("disqus-url" (ego--get-full-url uri))
             ("disqus-comment" (ego--get-config-option :personal-disqus-shortname))
             ("disqus-shortname" (ego--get-config-option :personal-disqus-shortname))
             ("duoshuo-comment" (ego--get-config-option :personal-duoshuo-shortname))
             ("duoshuo-shortname" (ego--get-config-option :personal-duoshuo-shortname))
             ("google-analytics" (ego--get-config-option :personal-google-analytics-id))
             ("google-analytics-id" (ego--get-config-option :personal-google-analytics-id))
             ("creator-info" (ego--get-html-creator-string))
             ("email" (ego--confound-email-address (or (ego--read-org-option "EMAIL")
                                                      user-mail-address
                                                      "Unknown Email"))))))))


(provide 'ego-template)

;;; ego-template.el ends here
