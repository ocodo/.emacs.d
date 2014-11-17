;;; waher-theme.el --- Emacs 24 theme based on waher for st2 by dduckster
;;; Author: Jasonm23 <jasonm23@gmail.com>
;;; Url: https://github.com/jasonm23/emacs-waher-theme
;; Version: 20141115.430
;;; X-Original-Version: 20141115.2020
;;; Package-Requires: ((emacs "24.1"))
;;
;;; Changelog:
;; 20141115.2020: - fixed rainbow delimiters and greater than emacs 24 version check
;; 20130917.0007: - updated for better xterm 256color terminal support
;; 20130830.0213: - initial version
;;
;;; License:
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, version 3 of the License.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.
;;
;; This file is not a part of Emacs
;;
;;; Commentary:
;; an Emacs 24 theme based on waher for st2 by dduckster
;; see http://www.dduckster.com/Blog/awesome/sublime-theme-dd-waher
;;

(unless (>= emacs-major-version 24)
  (error "waher-theme requires Emacs 24 or later."))

(deftheme waher
  "converted to emacs by jasonm23 - based on waher for st2 by dduckster")

(custom-theme-set-faces
 `waher

 `(cursor                              ((t ( :background "#f8f8f2" ))))
 `(default                             ((((class color) (min-colors 88)) (:background "#000000" :foreground "#CEDBE7"))
                                        (t ( :background "#252620" :foreground "#CEDBE7" ))))
 `(mode-line                           ((((class color) (min-colors 88)) (:background "#222222" :foreground "#CEDBE7"))
                                        (t ( :background "#292923" :foreground "#CEDBE7" :height 125 ))))
 `(linum                               ((((class color) (min-colors 88)) (:background "#000000" :foreground "#3E4D4A"))
                                        (t ( :background "#292923" :foreground "#3E4D4A" ))))
 `(trailing-whitespace                 ((t (                       :foreground "#3B3A32" ))))
 `(highlight                           ((t ( :background "#333333" ))))
 `(region                              ((t ( :background "#49483E" ))))
 `(cua-rectangle                       ((t ( :background "#59484E" ))))
 `(fringe                              ((t ( :background "#292A24" ))))
 `(isearch                             ((t ( :background "#FFE792" :foreground "#000000" ))))
 `(mode-line-inactive                  ((t ( :background "#292423" :foreground "#4E4D4A" ))))
 `(mode-line-emphasis                  ((t (                       :foreground "#CDEBF7" ))))
 `(mode-line-highlight                 ((t (                       :foreground "#CDEBF7" ))))
 `(minibuffer-prompt                   ((t (                       :foreground "#3090DE" ))) )
 `(font-lock-comment-face              ((t (                       :foreground "#7F9F7F" ))))
 `(font-lock-comment-delimiter-face    ((t (                       :foreground "#6F8F6F" ))))
 `(font-lock-string-face               ((t (                       :foreground "#8CBED6" ))))
 `(font-lock-constant-face             ((t (                       :foreground "#DEC77B" ))))
 `(font-lock-doc-face                  ((t (                       :foreground "#7F9F7F" ))))
 `(font-lock-builtin-face              ((t (                       :foreground "#DEC77B" ))))
 `(font-lock-keyword-face              ((t (                       :foreground "#DEC77B" ))))
 `(font-lock-variable-name-face        ((t (                       :foreground "#EFA252" ))))
 `(font-lock-type-face                 ((t (                       :foreground "#607060" :underline "#607060" ))))
 `(font-lock-function-name-face        ((t (                       :foreground "#D65921" ))))
 `(font-lock-warning-face              ((t (                       :foreground "#F92672" ))))
 `(font-lock-doc-string-face           ((t (                       :foreground "#7F9F7F" ))))
 `(font-lock-negation-char-face        ((t (                       :foreground "#F92F72" ))))
 `(font-lock-preprocessor-face         ((t (                       :foreground "#EC9A41" ))))
 `(font-lock-regexp-grouping-backslash ((t (                       :foreground "#DF6300" ))))
 `(font-lock-regexp-grouping-construct ((t (                       :foreground "#DEC777" ))))
 `(flymake-errline                     ((t (                                             :underline "#F92672" ))))
 `(flymake-warnline                    ((t (                                             :underline "#008833" ))))
 `(diff-added                          ((t (                       :foreground "#A6E22E" ))))
 `(diff-removed                        ((t (                       :foreground "#F92672" ))))
 `(diff-changed                        ((t (                       :foreground "#3377FF" ))))
 `(diff-file-header                    ((t (                       :foreground "#D65921" ))))
 `(diff-context                        ((t (                       :foreground "#DDDDDD" ))))
 `(diff-hunk-header                    ((t (                       :foreground "#75715E" ))))
 `(git-gutter+-added                   ((t (                       :foreground "#A6E22E" ))))
 `(git-gutter+-deleted                 ((t (                       :foreground "#F92672" ))))
 `(git-gutter+-modified                ((t (                       :foreground "#3377FF" ))))
 `(rainbow-delimiters-depth-1-face     ((t (                       :foreground "#D65921" ))))
 `(rainbow-delimiters-depth-2-face     ((t (                       :foreground "#2B88A8" ))))
 `(rainbow-delimiters-depth-3-face     ((t (                       :foreground "#FFA07E" ))))
 `(rainbow-delimiters-depth-4-face     ((t (                       :foreground "#30D039" ))))
 `(rainbow-delimiters-depth-5-face     ((t (                       :foreground "#58A0A2" ))))
 `(rainbow-delimiters-depth-6-face     ((t (                       :foreground "#6070DF" ))))
 `(rainbow-delimiters-depth-7-face     ((t (                       :foreground "#D78060" ))))
 `(rainbow-delimiters-depth-8-face     ((t (                       :foreground "#FFDD77" ))))
 `(rainbow-delimiters-depth-9-face     ((t (                       :foreground "#44FF88" ))))
 `(rainbow-delimiters-unmatched-face   ((t (                       :foreground "#F92672" ))))
 )

(custom-theme-set-variables 'waher)

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'waher)

;; Local Variables:
;; eval: (when (fboundp 'rainbow-mode) (rainbow-mode +1))
;; End:

;;; waher-theme.el ends here
