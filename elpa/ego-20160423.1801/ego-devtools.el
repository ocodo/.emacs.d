;;; ego-devtools.el --- Functions used to develop EGO

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

;; ego-devtools.el contains functions used to develop EGO.

;;; Code:
(require 'org)
(require 'ego-config)
(require 'ego-util)

(defun ego--devtools-update-config ()
  (interactive)
  (ego-add-to-alist
   'ego-project-config-alist
   `(("EGO"
     :repository-directory ,ego-load-directory
     :site-domain "http://emacs-china.github.io/EGO"
     :site-main-title "EGO"
     :site-sub-title "Static site generator based on Emacs, Git and Org-mode"
     :theme (default)
     :force-absolute-url t
     :source-browse-url ("GitHub" "https://github.com/emacs-china/EGO")
     :repository-org-branch "master"
     :repository-html-branch "gh-pages"
     :summary nil
     :confound-email t
     :ignore-file-name-regexp "readme.org"
     :web-server-docroot ,(expand-file-name "~/webRoot/EGO")
     :web-server-port 4321))))

(ego--devtools-update-config)

(provide 'ego-devtools)

;;; ego-devtools.el ends here
