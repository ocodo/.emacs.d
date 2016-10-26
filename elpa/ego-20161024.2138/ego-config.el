;;; ego-config.el --- Variables and Functions dealing with ego configure

;; Copyright (C)  2015 Feng Shu, Kuangdash
;;                2012, 2013, 2014, 2015 Kelvin Hu

;; Author: Feng Shu  <tumashu AT 163.com>
;;         Kelvin Hu <ini DOT kelvin AT gmail DOT com>
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

;; ego-config.el contains almost all variable definitions and configurations.

;;; Code:

(require 'ox)
(require 'ht)
(require 'subr-x)

(defgroup ego nil
  "Options for generating static pages using ego."
  :tag "Org static page generator"
  :group 'org)

(defcustom ego--default-project-name nil
  "If set, `ego-do-publication' will directly publish this project
and `ego-new-post' will directly add new post to this project."
  :group 'ego
  :type 'string)

(defcustom ego-project-config-alist nil
  "Association list to control ego publishing behavior.

Each element of the alist is a ego 'project.'  The CAR of
each element is a string, uniquely identifying the project.  The
CDR of each element is a well-formed property list with an even
number of elements, alternating keys and values, specifying
parameters for the publishing process.

  \(:property value :property value ... )

Most properties are optional, but some should always be set:


  `:repository-directory'

The git repository directory, where org files stored on branch
`:repository-org-branch', and generated html files stored on branch
`:repository-html-branch'.
1. Type: string
2. Example1: \"~/.emacs.d/projects/tumashu.github.com/\"


  `:site-domain'

The domain name of entire site, it is recommended to assign with prefix
http:// or https://, http will be considered if not assigned.
1. Type: string
2. Example1: \"http://tumashu.github.com\"


  `:site-main-title'

The main title of entire site.
1. Type: string
2. Example1: \"Tumashu's website\"


  `:site-sub-title'

The subtitle of entire site.
1. Type: string
2. Example1: \"======> My personal blog site.\"


  `:preparation-function'

Function to be called before publishing this project.  This may also
be a list of functions.
1. Type: function
2. Example: eh-convert-el-to-org


  `:repository-org-branch'

The branch where org files stored on, it is within repository presented by
`:repository-directory'.
1. Type: string
2. Example1: \"source\"
3. Example2: \"master\"


  `:repository-html-branch'

The branch where generated html files stored on, it is within repository
presented by `:repository-directory'.
1. Type: string
2. Example1: \"master\"
3. Example2: \"gh-pages\"


  `:theme-root-directory'

The root directory list that stores themes for page rendering. By default, it
points to the directory `themes' in ego installation directory.
1. Type: list
2. Example1: (\"/path/to/dir1\" \"/path/to/dir2\" \"/path/to/dir3\" ...)
3. Example2: nil

When set this option to `nil', ego will find two paths as fallback:
1. <Directory which contain ego.el>/themes
2. <Your project repository directory>/themes


  `:theme'

The theme used for page generation.
1. Type: list
2. Example1: (worg killjs)
3. Example2: nil

When set this option to `nil', default theme will be used.


  `:source-browse-url'

The personal github link.
1. Type: list
2. Example1: (\"GitHub\" \"https://github.com/emacs-china/ego\")


  `:personal-avatar'

The link to an avatar image.
1. Type: string
2. Example1: \"/media/img/horse.jpg\"
2. Example2: \"http://tumashu.github.com/ego-media/img/horse.jpg\"


  `:personal-disqus-shortname'

The personal disqus shortname.
1. Type: string
2. Example1: \"my-disqus-name\"


  `:personal-duoshuo-shortname'

The personal duoshuo shortname.
1. Type: string
2. Example1: \"my-duoshuo-name\"


  `:personal-google-analytics-id'

Personal google analytics id.
The personal duoshuo shortname.
1. Type: string
2. Example1: \"my-google-analytics-id\"


  `:confound-email'

Determine whether email addresses should be confounded or not.
1. Type: boolean
2. Example1: t

When set this option to `t', \"myname@163.com\" will be converted to \"myname <at> 163 <dot> com\"


  `:force-absolute-url'

Force convert relative url to absolute url in html files by append site domain.
1. Type: boolean
2. Example1: t

When set this option to `t', all url like \"/path/to/file.html\" will be
converted to \"http://yourdomain.com/path/to/file.html\".


  `:default-category'

If org files don't set category, default category will be used.
1. Type: string
2. Example1: \"blog\"
3. Example2: \"wiki\"
4. Example3: \"documents\"

  `:about'

About page of org-website.
1. Type: list
2. Example1: (\"About\" \"/about/\")


  `:rss'

RSS page of org-website.
1. Type: list
2. Example1: (\"RSS\" \"/rss.xml\")


  `:summary'

A summary is a statistic page, Which can be used show pages
based on \"tags\" , \"data\" , \"author\" and so on.
it is similar Microsoft Excel pivot table feature.
1. Type: alist
2. Example1: ((\"tags\" :tags) (\"years\" :year) (\"authors\" :authors))
2. Example2: ((\"按标签分类\" :tags) (\"按年度分类\" :year) (\"按作者分类\" :authors))


  `:category-ignore-list'

Ignore subdirs/categories for navigation.
1. Type: list
2. Example1: (\"themes\" \"assets\")

Names in this list will not showed in webpage navbar.


  `:ignore-file-name-regexp'

the file whose name(include path) match the regexp won't be exported.

  `:get-title-function'

A function used to retrieve an org file's Title, it has no parameter and
run org file buffer.
1. Type: function
2. Example1: ego--get-title


  `:retrieve-category-function'

A function used to retrieve an org file's category, its parameter is the
org file's path, if parameter is nil, it should return all categories.
1. Type: function
2. Example1: ego--get-file-category


   `:org-export-function'

Set the default function by which ego export org file to html.
1. Type: function
2. Example1: ego--default-org-export


  `:html-creator-string'

Information about the creator of the HTML document.
1. Type: string
2. Example1: \"This is an example creator string\"


  `:repo-files-function'

The function used to get all org files exported.
1. Type: function
2. Example1: ego--git-all-files


  `:addition-files-function'

The function used to get addition org files exported, for example:
org files ignored by git, which are generated from other files.
1. Type: function
2. Example1: ego--addition-all-files


  `:web-server-docroot'

ego can start a web server to test publish, this
set the server document root.
1. Type: string
2. Example1: \"~/.emacs.d/org-website-server/ego-\"


  `:web-server-port'

ego can start a web server to test publish, this
set the server port.
1. Type: number
2. Example1: 9876


You can see fallback value of above option in `ego-config-fallback'"
:group 'ego
:type 'alist)

(defcustom ego--get-config-option-function
  'ego--get-config-option-from-alist
  "The function used to get config option."
  :group 'ego
  :type 'function)

(defconst ego--temp-buffer-name "*EGO Output*"
  "Name of the temporary buffer used by ego.")

(defconst ego-load-directory
  (cond
   (load-file-name (file-name-directory load-file-name))
   ((symbol-file 'ego--temp-buffer-name)
    (file-name-directory (symbol-file 'ego--temp-buffer-name)))
   ((string= (file-name-nondirectory buffer-file-name) "ego-config.el")
    (file-name-directory buffer-file-name))
   (t nil))
  "The directory where ego is loaded from.")

(defvar ego--category-config-alist
  '(("blog"
     :show-meta t
     :show-comment t
     :uri-generator ego--generate-uri
     :uri-template "/blog/%y/%m/%d/%t/"
     :sort-by :date     ;; how to sort the posts
     :category-index nil) ;; generate category index or not
    ("index"
     :show-meta nil
     :show-comment nil
     :uri-generator ego--generate-uri
     :uri-template "/"
     :sort-by :date
     :category-index nil)
    ("about"
     :show-meta nil
     :show-comment nil
     :uri-generator ego--generate-uri
     :uri-template "/about/"
     :sort-by :date
     :category-index nil))
  "Configurations for different categories, can and should be customized.")

(defvar ego--category-show-list nil
  "the list of category names(string) which will be showed in the navigation-bar")

(defvar ego--current-project-name nil)
(defvar ego--last-project-name nil)

(defvar ego--publish-without-org-to-html nil
  "partial org-files publish without org-to-html: 1; all org-files publish without org-to-html: 2; others: nil")

(defvar ego--publish-to-repository nil
  "Mainly used in converting relative-url to absolute-url")

(defvar ego--item-cache nil
  "The cache for general purpose.")

(defvar ego--async-publish-success nil
  "When push remote success: t")

(defconst ego--rss-template "<?xml version=\"1.0\" encoding=\"utf-8\"?>
<rss version=\"2.0\">
  <channel>
    <title>{{title}}</title>
    <link>{{link}}</link>
    <description>{{description}}</description>
    <pubDate>{{date}}</pubDate>
    <lastBuildDate>{{date}}</lastBuildDate>
    <docs>http://www.rssboard.org/rss-specification</docs>
    <generator>ego static site generator \
(https://github.com/emacs-china/ego)</generator>
{{#items}}
<item>
<title>{{item-title}}</title>
<link>{{item-link}}</link>
<description>{{item-description}}</description>
<pubDate>{{item-update-date}}</pubDate>
<guid>{{item-link}}</guid>
</item>
{{/items}}
</channel>
</rss>"
  "Template for RSS rendering.")

(defvar ego-config-fallback
      `(:repository-directory nil
        :site-domain nil
        :site-main-title "ego"
        :site-sub-title "static site generator"
        :repository-org-branch "source"
        :repository-html-branch "master"
        :theme-root-directory nil
        :theme (default)
        :source-browse-url nil
        :personal-avatar nil
        :personal-disqus-shortname nil
        :personal-duoshuo-shortname nil
        :personal-google-analytics-id nil
        :default-category "blog"
        :about ("About" "/about/")
        :rss ("RSS" "/rss.xml")
        :category-ignore-list ("themes" "assets")
        :ignore-file-name-regexp "\n"
        :summary (("tags" :tags))
        :confound-email t
        :force-absolute-url t
        :preparation-function nil
        :get-title-function ego--get-title
        :retrieve-category-function ego--get-file-category
        :repo-files-function ego--git-all-files
        :addition-files-function nil
        :org-export-function ego--default-org-export
        :web-server-docroot "~/.emacs.d/ego-server/default"
        :web-server-port 9876
        :html-creator-string ,(format "<a href=\"http://www.gnu.org/software/emacs/\">Emacs</a> %s\
(<a href=\"http://orgmode.org\">Org mode</a> %s)"
(format "%s.x" emacs-major-version)
(if (fboundp 'org-version)
    (replace-regexp-in-string "\\..*" ".x" (org-version))
  "Unknown Version"))
"If User don't set an option, ego will use fallback value of this option."))

(defun ego--get-config-option (option)
  "The function used to read ego config"
  (when (functionp ego--get-config-option-function)
    (funcall ego--get-config-option-function option)))

(defun ego--get-config-option-from-alist (option)
  "The default ego config read function,
which can read `option' from `ego-project-config-alist'
if `option' is not found, get fallback value from
`ego-config-fallback'."
  (let ((project-plist (cdr (assoc ego--current-project-name
                                   ego-project-config-alist))))
    (if (plist-member project-plist option)
        (plist-get project-plist option)
      (plist-get ego-config-fallback option))))

(defun ego--get-repository-directory ()
  "The function, which can return repository directory string."
  (let ((dir (ego--get-config-option :repository-directory)))
    (when dir
      (file-name-as-directory
       (expand-file-name dir)))))

(defun ego--get-site-domain ()
  "The function, which can return site-domain string."
  (let ((site-domain (ego--get-config-option :site-domain)))
    (when site-domain
      (if (or (string-prefix-p "http://"  site-domain)
              (string-prefix-p "https://" site-domain))
          (directory-file-name
           (file-name-as-directory site-domain))
        (directory-file-name
         (file-name-as-directory
          (concat "http://" site-domain)))))))

(defun ego--get-theme-dirs (&optional root-dir theme type)
  "The function ,return ego theme type paths list.

ego organizes its themes by directory:

| Directory           |  Argument   |  Value                 |
+---------------------+-------------+------------------------+
| /path/to/directory  |  <root-dir> | \"/path/to/directory\" |
|  \--mdo             |  <theme>    | 'mdo                   |
|      |-- templates  |  <type>     | 'templates             |
|       \- resources  |  <type>     | 'resources             |

`root-dir' and `theme' can be lists, for example:

  `(\"path/to/dir1\" \"path/to/dir2\" \"path/to/dir3\")'
  `(theme1 theme2 theme3)'

At this time, `ego--get-theme-dirs' will find *all possible*
<type> directorys by permutation way and return a list with
multi path."
  (let* ((themes (delete-dups
                  (if theme
                      (list theme)
                    `(,@(ego--get-config-option :theme) default))))
         (theme-root-dirs (delete-dups
                           (if root-dir
                               (list root-dir)
                             `(,@(ego--get-config-option :theme-root-directory)
                               ,(concat (ego--get-repository-directory) "themes/")
                               ,(concat ego-load-directory "themes/")))))
         theme-dir theme-dirs)
    (dolist (theme themes)
      (dolist (root-dir theme-root-dirs)
        (setq theme-dir
              (file-name-as-directory
               (expand-file-name
                (format "%s/%s" (symbol-name theme)
                        (if type (symbol-name type) ""))
                root-dir)))
        (when (file-directory-p theme-dir)
          (push theme-dir theme-dirs))))
    (reverse theme-dirs)))

(defun ego--get-html-creator-string ()
  "The function, which can return creator string."
  (or (ego--get-config-option :html-creator-string) ""))

(defun ego--get-category-setting (category)
  "The function , which can return category config of `category'"
  (or (assoc category ego--category-config-alist)
      `(,category
        :show-meta t
        :show-comment t
        :uri-generator ego--generate-uri
        :uri-template ,(format "/%s/%%t/" category)
        :sort-by :date
        :category-index t)))

(provide 'ego-config)

;;; ego-config.el ends here
