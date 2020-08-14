;;; ego-export.el --- Publication related functions required by ego

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

;; org source publication related functions

;;; Code:

(require 'format-spec)
(require 'ox)
(require 'ht)
(require 'cl-lib)
(require 'dash)
(require 'ego-util)
(require 'ego-config)
(require 'ego-git)
(require 'ego-template)
(require 'files)
(require 'url-parse)


(defun ego--publish-changes (files-list addition-list change-plist pub-root-dir)
  "This function is for:
1. publish changed org files to html
2. delete html files which are relevant to deleted org files (NOT implemented)
3. update index pages
4. regenerate tag pages
`files-list' and `addition-list' contain paths of org files, `change-plist'
contains two properties, one is :update for files to be updated, another is :delete
for files to be deleted. `pub-root-dir' is the root publication directory."
  (let* ((repo-dir (ego--get-repository-directory))
         (upd-list (delete-dups
                    (append (plist-get change-plist :update)
                            addition-list)))
         (del-list (plist-get change-plist :delete))
         (files-list (delete-dups (append files-list addition-list))) ;files-list中只包含了当前仓库中所有的文件，不包含已经被删除的文件，因此需要把del-list也加入
          file-attr-list)
    (message "EGO DEBUG: del-list=[%s]" del-list)
    (when del-list
      (mapcar
                      (lambda (org-file)
                        (message "EGO DEBUG: org-file=[%s]" org-file)
                        (ego--handle-deleted-file org-file)
                        (ego-delete-org-html-mapping org-file 'del))
                      del-list))
    (message "EGO DEBUG: upd-list=[%s]" upd-list)
    (when upd-list
      (setq file-attr-list
            (reverse (mapcar
                      (lambda (org-file)
                        (message "EGO DEBUG: org-file=[%s]" org-file)
                        (let* ((need-upd-p (member org-file upd-list)))
                          (let* ((attr-cell (ego--get-org-file-options
                                             org-file
                                             pub-root-dir
                                             need-upd-p))
                                 (attr-plist (car attr-cell))
                                 (component-table (cdr attr-cell)))
                            (when need-upd-p
                              (run-hook-with-args 'ego-pre-publish-hooks attr-plist)
                              (let ((new-html-uri (ego--publish-modified-file component-table
                                                                              (plist-get attr-plist :pub-dir))))
                                (ego-update-org-html-mapping org-file new-html-uri 'del))
                              (run-hook-with-args 'ego-post-publish-hooks attr-plist))
                            attr-plist)))
                      files-list)))
      (unless (member
               (expand-file-name "index.org" repo-dir)
               files-list)
        (ego--generate-default-index file-attr-list pub-root-dir))
      (when (and (ego--get-config-option :about)
                 (not (member
                       (expand-file-name "about.org" repo-dir)
                       files-list)))
        (ego--generate-default-about pub-root-dir))
      (ego--update-category-index file-attr-list pub-root-dir)
      (when (ego--get-config-option :rss)
        (ego--update-rss file-attr-list pub-root-dir))
      (mapc
       (lambda (name)
           (ego--update-summary file-attr-list pub-root-dir name))
       (mapcar #'car (ego--get-config-option :summary))))))

(defun ego--generate-description ()
  "Generate description of current org file buffer."
  (save-excursion
    (goto-char (point-min))
    (search-forward-regexp "^[^#]" nil t)
    (buffer-substring-no-properties (point) (point-max))))

(defun ego--get-org-file-options (org-file pub-root-dir do-pub)
  "Retrieve all needed options for org file opened in current buffer.
PUB-ROOT-DIR is the root directory of published files, if DO-PUB is t, the
content of the buffer will be converted into html.

This functions works with `ego-current-project-name' and currect buffer.
返回值包括两部分:(cons attr-plist component-table),其中attr-plist是元信息，component-table是渲染后的HTML内容"
  (let* ((visiting-p (find-buffer-visiting org-file))
         (file-buffer (or visiting-p (find-file org-file)))
         result)
    (with-current-buffer file-buffer
      (let* ((repo-dir (ego--get-repository-directory))
             (filename (buffer-file-name))
             (title (funcall (ego--get-config-option :get-title-function)))
             (date (ego--fix-timestamp-string
                    (or (ego--read-org-option "DATE")
                        (ego-git-first-change-date repo-dir filename)
                        (format-time-string "%Y-%m-%d"))))
             (year (format "%.4s" date))
             (mod-date (if (not filename)
                           (format-time-string "%Y-%m-%d")
                         (or (ego-git-last-change-date
                              repo-dir
                              filename)
                             (format-time-string
                              "%Y-%m-%d"
                              (nth 5 (file-attributes filename))))))
             (description (or (ego--read-org-option "DESCRIPTION")
                              (ego--generate-description)
                              "No Description"))
             (thumb (ego--read-org-option "THUMBNAIL"))
             (tags (ego--read-org-option "TAGS"))
             (tags (when tags
                     (delete "" (mapcar 'string-trim
                                        (split-string tags "[:,]+" t)))))
             (authors (ego--read-org-option "AUTHOR"))
             (authors (when authors
                        (delete "" (mapcar 'string-trim
                                           (split-string authors "[:,]+" t)))))
             (category (ego--get-category filename))
             (cat-config (cdr (or (assoc category ego--category-config-alist)
                                  (ego--get-category-setting
                                   (ego--get-config-option :default-category)))))
             (uri (funcall (plist-get cat-config :uri-generator)
                           (plist-get cat-config :uri-template)
                           date
                           title))
             (pub-dir (file-name-as-directory
                       (concat
                        (file-name-as-directory pub-root-dir)
                        (replace-regexp-in-string
                         "\\`/" ""
                         uri))))
             (attr-plist `(:source-file ,(expand-file-name org-file)
                                        :title ,title
                                        :date ,date
                                        :mod-date ,mod-date
                                        :description ,description
                                        :thumb ,thumb
                                        :year ,year
                                        :tags ,tags
                                        :authors ,authors
                                        :category ,category
                                        ;; ADD index.html for uri if uri is a directory
                                        :uri ,(if (string-suffix-p ".html" uri)
                                                  uri
                                                (concat (file-name-as-directory uri) "index.html"))
                                        :pub-dir ,pub-dir))
             component-table)
        (when do-pub
          (princ attr-plist)
          (let* ((post-content (ego--render-content))
                 (assets-dir (file-name-as-directory
                              (concat (file-name-as-directory pub-root-dir)
                                      "assets/"
                                      (replace-regexp-in-string
                                       "\\`" "" uri)))))
            (with-temp-buffer
              (insert post-content)
              (goto-char (point-min))
              (while (re-search-forward
;;; TODO: not only links need to convert, but also inline
;;; images, may add others later
                      ;; "<a[^>]+href=\"\\([^\"]+\\)\"[^>]*>\\([^<]*\\)</a>" nil t)
                      "<[a-zA-Z]+[^/>]+\\(src\\|href\\)=\"\\([^\"]+\\)\"[^>]*>" nil t)
                (let* ((asset-path (match-string 2))
                       (asset-path-begin (match-beginning 2))
                       (asset-path-end (match-end 2))
                       (asset-abs-path (expand-file-name asset-path (file-name-directory filename)))
                       (pub-abs-path (concat assets-dir (file-name-nondirectory asset-path))))
                  (unless (url-type (url-generic-parse-url asset-path)) ;; 判断是否为绝对路径的URI
                    (if (not (file-exists-p asset-abs-path))
                        (message "EGO: [WARN] File %s in hyper link does not exist, org \
file: %s." asset-abs-path filename)
                      (unless (file-directory-p assets-dir)
                        (mkdir assets-dir t))
                      (message "DEBUG EGO: asset-abs-path=[%s],assets-dir=[%s]" asset-abs-path assets-dir)
                      (copy-file asset-abs-path assets-dir t t t t)
                      (ego-update-org-html-mapping asset-abs-path
                                                   (expand-file-name (file-name-nondirectory asset-abs-path) assets-dir)
                                                   'del) ;附件也要记录下来
                      (unless (string-prefix-p pub-root-dir pub-abs-path)
                        (message "EGO: [WARN] The publication root directory %s is not an \
ancestor directory of assets directory %s." pub-root-dir assets-dir))
                      (let ((converted-path
                             (concat "/" (file-relative-name pub-abs-path pub-root-dir))))
                        (setf (buffer-substring asset-path-begin asset-path-end) converted-path))))))
              (setq post-content (buffer-string)))
            (setq component-table (ht ("header" (ego--render-header))
                                      ("nav" (ego--render-navigation-bar))
                                      ("content" post-content)
                                      ("footer" (ego--render-footer))))))
        (setq result (cons attr-plist component-table))))
    (unless visiting-p
      (kill-buffer file-buffer))
    result))

(defun ego--read-org-option (option)
  "Read option value of org file opened in *current buffer*.
e.g:
#+TITLE: this is title
will return \"this is title\" if OPTION is \"TITLE\""
  (let ((match-regexp (org-make-options-regexp `(,option))))
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward match-regexp nil t)
        (match-string-no-properties 2 nil)))))

(defun ego--generate-uri (default-uri-template creation-date title)
  "Generate URI of org file opened in current buffer. It will be firstly created
by #+URI option, if it is nil, DEFAULT-URI-TEMPLATE will be used to generate the
uri. If CREATION-DATE is nil, current date will be used. The uri template option
can contain following parameters:
%y: year of creation date
%m: month of creation date
%d: day of creation date
%t: title of current buffer
%f: file name of current buffer"
  (let ((uri-template (or (ego--read-org-option "URI")
                        default-uri-template))
      (date-list (split-string (if creation-date
                                   (ego--fix-timestamp-string creation-date)
                                 (format-time-string "%Y-%m-%d"))
                               "-"))
      (encoded-title (ego--encode-string-to-url title))
      (encoded-file-name (ego--encode-string-to-url
                          ;; 获取buffer的文件名
                          (replace-regexp-in-string "^.*/\\|\\.org$" "" (buffer-file-name)))))
  (format-spec uri-template `((?y . ,(car date-list))
                              (?m . ,(cadr date-list))
                              (?d . ,(cl-caddr date-list))
                              (?t . ,encoded-title)
                              (?f . ,encoded-file-name)))))


(defun ego--get-file-category (org-file)
  "This is the default function used to get a file's category,
see ego config option 'retrieve-category-function. How to judge a
file's category is based on its name and its root folder name."
  (let ((repo-dir (ego--get-repository-directory))
        (default-category (ego--get-config-option :default-category))
        (category-ignore-list (ego--get-config-option :category-ignore-list)))
    (cond ((not org-file)
           (let ((default-categories `("index" "about" ,(ego--get-config-option :default-category)))
                 (cat-list (cl-remove-if-not (lambda (f)
                                               (and (not (equal f "."))
                                                    (not (equal f ".."))
                                                    (not (equal f ".git"))
                                                    (not (member f category-ignore-list))
                                                    (not (equal f default-category))
                                                    (file-directory-p
                                                     (expand-file-name f repo-dir))))
                                             (directory-files repo-dir))))
             `(,@cat-list ,@default-categories)))
          ((string= (expand-file-name "index.org" repo-dir)
                    (expand-file-name org-file)) "index")
          ((string= (expand-file-name "about.org" repo-dir)
                    (expand-file-name org-file)) "about")
          ((string= (file-name-directory (expand-file-name org-file))
                    repo-dir) default-category)
          (t (car (split-string (file-relative-name
                                 (expand-file-name org-file) repo-dir)
                                "[/\\\\]+"))))))


(defun ego--copy-file-handler (operation &rest args)
  "Use `ego--html-link-transformer' function in export process for htm/html files"
  ;; First check for the specific operations
  ;; that we have special handling for.
  (let ((source-file (nth 0 args))
        (dest-file (nth 1 args)))
    (cond ((eq operation 'copy-file)
           (ego--string-to-file
            (ego--html-link-transformer (ego--file-to-string source-file) dest-file)
            dest-file))
          ;; Handle any operation we don’t know about.
          (t (let ((inhibit-file-name-handlers
                    (cons 'ego--copy-file-handler
                          (and (eq inhibit-file-name-operation operation)
                               inhibit-file-name-handlers)))
                   (inhibit-file-name-operation operation))
               (apply operation args))))))

(defun ego--publish-modified-file (component-table pub-path)
  "Publish org file opened in current buffer. COMPONENT-TABLE is the hash table
used to render the template, PUB-PAHT is the directory for published html file or the published html file itself(with .html suffix).
If COMPONENT-TABLE is nil, the publication will be skipped."
  (when component-table
    (let* ((uri (if (string-suffix-p ".html" pub-path)
                    pub-path
                  (concat (file-name-as-directory pub-path) "index.html")))
           (pub-dir (file-name-directory uri)))
      (unless (file-directory-p pub-dir)
        (mkdir pub-dir t))
      (ego--save-to-file
       (mustache-render
        (ego--get-cache-create
         :container-template
         (message "EGO: Read container.mustache from file")
         (ego--file-to-string (ego--get-template-file "container.mustache"))) ; 模板的入口就是container.mustache
        component-table)
       uri)
      uri)))                            ;返回生成的HTML路径

(defun ego--handle-deleted-file (org-file-path)
  "TODO: add logic for this function, maybe a little complex."
  )

(defun ego--rearrange-category-sorted (file-attr-list)
  "Rearrange and sort attribute property lists from FILE-ATTR-LIST. Rearrange
according to category, and sort according to :sort-by property defined in
`ego--category-config-alist', if category is not in `ego--category-config-alist',
by default, category which set by config option `:default-category' will be used.
For sorting, later lies headmost."
  (let ((default-category (ego--get-config-option :default-category))
        cat-alist cat-list)
    (mapc
     (lambda (plist)
       (setq cat-list (cdr (assoc (plist-get plist :category) cat-alist)))
       (if cat-list
           (nconc cat-list (list plist))
         (setq cat-alist (cons (cons (plist-get plist :category)
                                     (list plist))
                               cat-alist))))
     file-attr-list)
    (mapc
     (lambda (cell)
       (setcdr
        cell
        (sort (cdr cell)
              (lambda (plist1 plist2)
                (<= (ego--compare-standard-date
                     (ego--fix-timestamp-string
                      (plist-get
                       plist1
                       (plist-get
                        (cdr (or (assoc (plist-get plist1 :category)
                                        ego--category-config-alist)
                                 (ego--get-category-setting default-category)))
                        :sort-by)))
                     (ego--fix-timestamp-string
                      (plist-get
                       plist2
                       (plist-get
                        (cdr (or (assoc (plist-get plist2 :category)
                                        ego--category-config-alist)
                                 (ego--get-category-setting default-category)))
                        :sort-by))))
                    0)))))
     cat-alist)))

(defun ego--update-category-index (file-attr-list pub-base-dir)
  "Update index page of different categories. FILE-ATTR-LIST is the list of all
file attribute property lists. PUB-BASE-DIR is the root publication directory."
  (let* ((sort-alist (ego--rearrange-category-sorted file-attr-list))
         (default-category (ego--get-config-option :default-category))
         cat-dir)
    (mapc
     (lambda (cat-list)
       (when (if (string= (car cat-list) "blog")
                 t
               (member (car cat-list) (ego-get-category-show-list)))
         (setq cat-dir (file-name-as-directory
                        (concat (file-name-as-directory pub-base-dir)
                                (ego--encode-string-to-url (car cat-list)))))
         (unless (file-directory-p cat-dir)
           (mkdir cat-dir t))
         (ego--save-to-file
          (mustache-render
           (ego--get-cache-create
            :container-template
            (message "EGO: Read container.mustache from file")
            (ego--file-to-string (ego--get-template-file "container.mustache")))
           (ht ("header"
                (ego--render-header
                 (ht ("page-title" (concat (capitalize (car cat-list))
                                           " Index - "
                                           (ego--get-config-option :site-main-title)))
                     ("author" (or user-full-name "Unknown Author")))))
               ("nav" (ego--render-navigation-bar))
               ("content"
                (ego--render-content
                 "category-index.mustache"
                 (ht ("cat-name" (capitalize (car cat-list)))
                     ("posts"
                      (mapcar
                       (lambda (attr-plist)
                         (let ((tags-multi (mapcar
                                            (lambda (tag-name)
                                              (ht ("link" (ego--generate-summary-uri "tags" tag-name))
                                                  ("name" tag-name)))
                                            (plist-get attr-plist :tags))))
                           (ht ("date"
                                (plist-get
                                 attr-plist
                                 (plist-get
                                  (cdr (or (assoc
                                            (plist-get attr-plist :category)
                                            ego--category-config-alist)
                                           (ego--get-category-setting default-category)))
                                  :sort-by)))
                               ("post-uri" (plist-get attr-plist :uri))
                               ("post-title" (plist-get attr-plist :title))
                               ("tag-links" (if (not tags-multi) "N/A"
                                              (mapconcat
                                               (lambda (tag)
                                                 (mustache-render
                                                  "<a href=\"{{link}}\">{{name}}</a>" tag))
                                               tags-multi " : "))))))
                       (cdr cat-list))))))
               ("footer"
                (ego--render-footer
                 (ht ("show-meta" nil)
                     ("show-comment" nil)
                     ("author" (or user-full-name "Unknown Author"))
                     ("google-analytics" (ego--get-config-option :personal-google-analytics-id))
                     ("google-analytics-id" (ego--get-config-option :personal-google-analytics-id))
                     ("creator-info" (ego--get-html-creator-string))
                     ("email" (ego--confound-email-address (or user-mail-address
                                                               "Unknown Email")))
                     )))))
          (concat cat-dir "index.html"))))
     sort-alist)))

(defun ego--generate-default-index (file-attr-list pub-base-dir)
  "Generate default index page, only if index.org does not exist. FILE-ATTR-LIST
is the list of all file attribute property lists. PUB-BASE-DIR is the root
publication directory."
  (let ((sort-alist (ego--rearrange-category-sorted file-attr-list))
        (id 0))
    (ego--save-to-file
     (mustache-render
      (ego--get-cache-create
       :container-template
       (message "EGO: Read container.mustache from file")
       (ego--file-to-string (ego--get-template-file "container.mustache")))
      (ht ("header"
           (ego--render-header
            (ht ("page-title" (concat "Index - " (ego--get-config-option :site-main-title)))
                ("author" (or user-full-name "Unknown Author")))))
          ("nav" (ego--render-navigation-bar))
          ("content"
           (ego--render-content
            "index.mustache"
            (ht ("categories"
                 (mapcar
                  (lambda (cell)
                    (ht ("id" (setq id (+ id 1)))
                        ("category" (car cell))
                        ("posts" (mapcar
                                  (lambda (plist)
                                    (ht ("post-uri"
                                         (plist-get plist :uri))
                                        ("post-title"
                                         (plist-get plist :title))
                                        ("post-desc"
                                         (plist-get plist :description))
                                        ("post-date"
                                         (plist-get plist :date))
                                        ("post-thumb"
                                         (or (plist-get plist :thumb) ""))))
                                  (cdr cell)))))
                  sort-alist)))))
          ("footer"
           (ego--render-footer
            (ht ("show-meta" nil)
                ("show-comment" nil)
                ("author" (or user-full-name "Unknown Author"))
                ("google-analytics" (ego--get-config-option :personal-google-analytics-id))
                ("google-analytics-id" (ego--get-config-option :personal-google-analytics-id))
                ("creator-info" (ego--get-html-creator-string))
                ("email" (ego--confound-email-address (or user-mail-address
                                                          "Unknown Email"))))))))
     (concat (file-name-as-directory pub-base-dir) "index.html"))))

(defun ego--generate-default-about (pub-base-dir)
  "Generate default about page, only if about.org does not exist. PUB-BASE-DIR
is the root publication directory."
  (let* ((about-sub-dir
          (replace-regexp-in-string
           "^/" ""
           (car (cdr (ego--get-config-option :about)))))
         (pub-dir (file-name-as-directory
                   (expand-file-name about-sub-dir pub-base-dir))))
    (unless (file-directory-p pub-dir)
      (mkdir pub-dir t))
    (ego--save-to-file
     (mustache-render
      (ego--get-cache-create
       :container-template
       (message "EGO: Read container.mustache from file")
       (ego--file-to-string (ego--get-template-file "container.mustache")))
      (ht ("header"
           (ego--render-header
            (ht ("page-title" (concat "About - " (ego--get-config-option :site-main-title)))
                ("author" (or user-full-name "Unknown Author")))))
          ("nav" (ego--render-navigation-bar))
          ("content"
           (ego--render-content
            "about.mustache"
            (ht ("author" (or user-full-name "Unknown Author")))))
          ("footer"
           (ego--render-footer
            (ht ("show-meta" nil)
                ("show-comment" nil)
                ("author" (or user-full-name "Unknown Author"))
                ("google-analytics" (ego--get-config-option :personal-google-analytics-id))
                ("google-analytics-id" (ego--get-config-option :personal-google-analytics-id))
                ("creator-info" (ego--get-html-creator-string))
                ("email" (ego--confound-email-address (or user-mail-address
                                                          "Unknown Email"))))))))
     (concat pub-dir "index.html"))))

(defun ego--generate-summary-uri (summary-name summary-item-name)
  "Generate summary uri based on `summary-name' and `summary-item-name'."
  (concat "/" summary-name "/" (ego--encode-string-to-url summary-item-name)))

(defun ego--update-summary (file-attr-list pub-base-dir summary-name)
  "Update summary pages which name is `summary-name', FILE-ATTR-LIST
is the list of all file attribute property lists. PUB-BASE-DIR is the
root publication directory.

TODO: improve this function."
  (let* ((summary-base-dir (expand-file-name
                            (concat summary-name "/")
                            pub-base-dir))
         (summary-update-number (car (cddr (cdr (assoc summary-name (ego--get-config-option :summary))))))
         summary-alist summary-list summary-dir)
    (mapc
     (lambda (attr-plist)
       (mapc
        (lambda (name)
          (setq summary-list (assoc name summary-alist))
          (unless summary-list
            (add-to-list 'summary-alist (setq summary-list `(,name))))
          (nconc summary-list (list attr-plist)))
        (let* ((summary-attr (car (cdr (assoc summary-name (ego--get-config-option :summary)))))
               (elem (plist-get attr-plist summary-attr)))
          (if (listp elem) elem (list elem)))))
     file-attr-list)
    (when (equal summary-name (caar (-filter (lambda (element) (equal :tags (cadr element)))
                                             (ego--get-config-option :summary))))
      (setq summary-name "tags")
      (setq summary-base-dir (expand-file-name
                              (concat summary-name "/")
                              pub-base-dir)))
    (unless (file-directory-p summary-base-dir)
      (mkdir summary-base-dir t))
    (setq summary-alist (sort summary-alist
                              (lambda (plist1 plist2)
                                (string< (car plist1) (car plist2))))); sort the summary-item-name
    (mapc
     (lambda (summary-list)
       (setcdr
        summary-list
        (sort (cdr summary-list)
              (lambda (plist1 plist2)
                (<= (ego--compare-standard-date
                     (ego--fix-timestamp-string
                      (plist-get
                       plist1
                       :date))
                     (ego--fix-timestamp-string
                      (plist-get
                       plist2
                       :date)))
                    0)))))
     summary-alist); sort the summary post list according to the date
    (ego--save-to-file
     (mustache-render
      (ego--get-cache-create
       :container-template
       (message "EGO: Read container.mustache from file")
       (ego--file-to-string (ego--get-template-file "container.mustache")))
      (ht ("header"
           (ego--render-header
            (ht ("page-title" (concat (capitalize summary-name)
                                      " Index - "
                                      (ego--get-config-option :site-main-title)))
                ("author" (or user-full-name "Unknown Author")))))
          ("nav" (ego--render-navigation-bar))
          ("content"
           (ego--render-content
            "summary-index.mustache"
            (ht ("summary-name" (capitalize summary-name))
                ("updates-p" (numberp summary-update-number))
                ("updates"
                 (when (numberp summary-update-number)
                   (mapcar
                    (lambda (attr-plist)
                      (let ((tags-multi (mapcar
                                         (lambda (tag-name)
                                           (ht ("link" (ego--generate-summary-uri "tags" tag-name))
                                               ("name" tag-name)))
                                         (plist-get attr-plist :tags))))
                        (ht ("post-uri" (plist-get attr-plist :uri))
                            ("post-title" (plist-get attr-plist :title))
                            ("post-date" (plist-get attr-plist :mod-date))
                            ("tag-links" (if (not tags-multi) "N/A"
                                           (mapconcat
                                            (lambda (tag)
                                              (mustache-render
                                               "<a href=\"{{link}}\">{{name}}</a>" tag))
                                            tags-multi " : "))))))
                    (-uniq (-take
                            summary-update-number
                            (sort (do ((k summary-alist (cdr k))
                                       (result-k nil (append (cdr (car k)) result-k)))
                                      ((equal k nil) result-k))
                                  (lambda (plist1 plist2)
                                    (< (ego--compare-standard-date
                                        (ego--fix-timestamp-string
                                         (plist-get
                                          plist1
                                          :mod-date))
                                        (ego--fix-timestamp-string
                                         (plist-get
                                          plist2
                                          :mod-date)))
                                       0)))
                            )))))
                ("summary"
                 (mapcar
                  (lambda (summary-list)
                    (ht ("summary-item-name" (car summary-list))
                        ("summary-item-uri" (ego--generate-summary-uri summary-name (car summary-list)))
                        ("count" (number-to-string (length (cdr summary-list))))))
                  summary-alist)))))
          ("footer"
           (ego--render-footer
            (ht ("show-meta" nil)
                ("show-comment" nil)
                ("author" (or user-full-name "Unknown Author"))
                ("google-analytics" (ego--get-config-option :personal-google-analytics-id))
                ("google-analytics-id" (ego--get-config-option :personal-google-analytics-id))
                ("creator-info" (ego--get-html-creator-string))
                ("email" (ego--confound-email-address (or user-mail-address
                                                          "Unknown Email")))
                )))))
     (concat summary-base-dir "index.html"))
    (mapc
     (lambda (summary-list)
       (setq summary-dir (file-name-as-directory
                          (concat summary-base-dir
                                  (ego--encode-string-to-url (car summary-list)))))
       (unless (file-directory-p summary-dir)
         (mkdir summary-dir t))
       (ego--save-to-file
        (mustache-render
         (ego--get-cache-create
          :container-template
          (message "EGO: Read container.mustache from file")
          (ego--file-to-string (ego--get-template-file "container.mustache")))
         (ht ("header"
              (ego--render-header
               (ht ("page-title" (concat (capitalize summary-name) ": " (car summary-list)
                                         " - " (ego--get-config-option :site-main-title)))
                   ("author" "ego"))))
             ("nav" (ego--render-navigation-bar))
             ("content"
              (ego--render-content
               "summary.mustache"
               (ht ("summary-name" (capitalize summary-name))
                   ("summary-item-name" (car summary-list))
                   ("summary"
                    (mapcar
                     (lambda (summary-list)
                       (ht ("summary-item-name" (car summary-list))
                           ("summary-item-uri" (ego--generate-summary-uri summary-name (car summary-list)))
                           ("count" (number-to-string (length (cdr summary-list))))))
                     summary-alist))
                   ("posts"
                    (mapcar
                     (lambda (attr-plist)
                       (let ((tags-multi (mapcar
                                          (lambda (tag-name)
                                            (ht ("link" (ego--generate-summary-uri "tags" tag-name))
                                                ("name" tag-name)))
                                          (plist-get attr-plist :tags))))
                         (ht ("post-uri" (plist-get attr-plist :uri))
                             ("post-title" (plist-get attr-plist :title))
                             ("post-date" (plist-get attr-plist :date))
                             ("tag-links" (if (not tags-multi) "N/A"
                                            (mapconcat
                                             (lambda (tag)
                                                 (mustache-render
                                                  "<a href=\"{{link}}\">{{name}}</a>" tag))
                                             tags-multi " : "))))))
                     (cdr summary-list))))))
             ("footer"
              (ego--render-footer
               (ht ("show-meta" nil)
                   ("show-comment" nil)
                   ("author" (or user-full-name "Unknown Author"))
                   ("google-analytics" (ego--get-config-option :personal-google-analytics-id))
                   ("google-analytics-id" (ego--get-config-option :personal-google-analytics-id))
                   ("creator-info" (ego--get-html-creator-string))
                   ("email" (ego--confound-email-address (or user-mail-address
                                                             "Unknown Email")))
                   )))))
        (concat summary-dir "index.html")))
     summary-alist)))

(defun ego--update-rss (file-attr-list pub-base-dir)
  "Update RSS. FILE-ATTR-LIST is the list of all file attribute property lists.
PUB-BASE-DIR is the root publication directory."
  (let* ((rss-file-name
          (replace-regexp-in-string
           "^/" ""
           (car (cdr (ego--get-config-option :rss)))))
         (rss-file
          (concat (file-name-as-directory pub-base-dir) rss-file-name))
         (rss-base-dir
          (file-name-directory rss-file))
         (last-10-posts
          (-take 10 (--sort (>= 0 (ego--compare-standard-date
                                   (ego--fix-timestamp-string
                                    (plist-get it :mod-date))
                                   (ego--fix-timestamp-string
                                    (plist-get other :mod-date))))
                            (--filter (not (or
                                            (string= (plist-get it :category)
                                                     "index")
                                            (string= (plist-get it :category)
                                                     "about")))
                                      file-attr-list)))))
    (unless (file-directory-p rss-base-dir)
      (mkdir rss-base-dir t))
    (ego--save-to-file
     (mustache-render
      ego--rss-template
      (ht ("title" (ego--get-config-option :site-main-title))
          ("link" (ego--get-site-domain))
          ("description" (ego--get-config-option :site-sub-title))
          ("date" (format-time-string "%a, %d %b %Y %T %Z"))
          ("items" (--map (ht ("item-title" (plist-get it :title))
                              ("item-link" (ego--get-full-url (plist-get it :uri)))
                              ("item-description" (plist-get it :description))
                              ("item-update-date" (plist-get it :mod-date)))
                          last-10-posts))))
     rss-file)))

(org-add-link-type "ego-link"
                   'org-open-file
                   (lambda (path desc format)
                     (cond
                      ((eq format 'html) (ego-link-type-process-html path desc))
                      ((eq format 'latex) "This ego-link haven't been implementted"))))

;;;###autoload
(defun ego-link-type-process-html (path desc)
  "Generate EGO-LINK for html export, WARNING: EGO-LINK can only be linked to files in the repository directory"
  (let* ((default-directory (ego--get-repository-directory))
         (org-file-1 (expand-file-name path))
         (org-file-2 (car (file-expand-wildcards (format "**/*%s" path))))
         (org-file (cond ((file-exists-p org-file-1)
                          org-file-1)
                         ((file-exists-p org-file-2)
                          org-file-2)
                         (t (error "Can't find this ego-link!"))))
         (webpath (plist-get (car (ego--get-org-file-options org-file default-directory nil))
                             :uri)))
    (format "<span class=\"ego_link\"><a href=\"%s\">%s</a></span>" webpath desc)))

;;;###autoload
(defun org-ego-link-complete-link (&optional arg)
  "Completion function for EGO-LINK. ARG does nothing."
  (let* ((org-file (file-relative-name (read-file-name "enter file: " nil nil t)))
         (current-path (expand-file-name (buffer-file-name)))
          next-link-name)
    (when (y-or-n-p "Is it a PERVOUS(bi-directional) link? ")
      (let ((visiting-p (find-buffer-visiting org-file))
            (file-buffer (or visiting-p (find-file org-file)))
            (next-link-name (read-string "Set the NEXT link name:" "Next-Link" nil "Next-Link" t)))
        (with-current-buffer (switch-to-buffer file-buffer)
          (local-set-key "l" (lambda ()
                               (interactive)
                               (insert (format "[[ego-link:%s][%s]]" ,(file-relative-name current-path) ,next-link-name))
                               (local-unset-key "l")
                               (save-buffer)
                               (exit-recursive-edit)))
          (message "EGO: Press 'l' to insert this '%s'\n then input description for  the PERVOUS link" next-link-name)
          (recursive-edit))
        (unless visiting-p
          (kill-buffer file-buffer))))
    (format "ego-link:%s" org-file)))

(ignore-errors                            ;make EGO compatible with org-mode 8.x
  (org-link-set-parameters "ego-link"     ;function used by org-mode 9.x
                         :follow 'org-open-file
                         :export (lambda (path desc format)
                                   (cond
                                    ((eq format 'html) (ego-link-type-process-html path desc))
                                    ((eq format 'latex) "This ego-link haven't been implementted")))
                         :complete 'org-ego-link-complete-link
                         )
 )

(provide 'ego-export)

;;; ego-export.el ends here
