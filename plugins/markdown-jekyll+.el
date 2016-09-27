;;; markdown-jekyll+.el --- Code block highlighting for Markdown/Jekyll

;; Author: Jason Milkins <jasonm23@gmail.com>

;; URL: https://github.com/ocodo/.emacs.d/plugins/markdown-jekyll+.el

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
;;  Highlight Jekyll/Liquid code blocks
;;

;;; Code:

(require 'markdown-mode)

(defconst markdown-regex-jekyll-code-block-open
  "^{% highlight .*? %}$"
  "Regular expression matching opening of jekyll code blocks.")

(defconst markdown-regex-jekyll-code-block-close
  "^{% endhighlight %}$"
  "Regular expression matching closing of jekyll code blocks.")

;; add to markdown-fenced-block-pairs
(add-to-list 'markdown-fenced-block-pairs
             '((,markdown-regex-jekyll-code-block-open markdown-jekyll-block-begin)
               (,markdown-regex-jekyll-code-block-close markdown-jekyll-block-end)
               markdown-jekyll-code))

;; Add to markdown-syntax-properties
(plist-put 'markdown-syntax-properties 'markdown-jekyll-block-begin nil)
(plist-put 'markdown-syntax-properties 'markdown-jekyll-block-end nil)
(plist-put 'markdown-syntax-properties 'markdown-jekyll-code nil)

;; Add to markdown-mode-font-lock-keywords-basic
(append
 (list (cons markdown-match-jekyll-open-code-blocks '((0 markdown-markup-face))))
 markdown-mode-font-lock-keywords-basic)

(append
 (list (cons markdown-match-jekyll-close-code-blocks '((0 markdown-markup-face))))
 markdown-mode-font-lock-keywords-basic)

(append
 (list (cons markdown-match-jekyll-code-blocks '((0 markdown-pre-face))))
 markdown-mode-font-lock-keywords-basic)

(defun markdown-match-jekyll-code-blocks (last)
  "Match JEKYLL quoted code blocks from point to LAST.
Use data stored in 'markdown-jekyll-code text property during syntax
analysis."
  (markdown-match-propertized-text 'markdown-jekyll-code last))

(defun markdown-match-jekyll-open-code-blocks (last)
  "Match jekyll open block LAST."
  (markdown-match-propertized-text 'markdown-jekyll-block-begin last))

(defun markdown-match-jekyll-close-code-blocks (last)
  "Match jekyll close block LAST."
  (markdown-match-propertized-text 'markdown-jekyll-block-end last))

(markdown-reload-extensions)

(provide 'markdown-jekyll+)

;;; markdown-jekyll+.el ends here
