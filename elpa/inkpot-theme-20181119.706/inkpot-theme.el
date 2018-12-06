;;; inkpot-theme.el --- port of vim's inkpot theme

;; Author: Sarah Iovan <sarah@hwaetageek.com>
;;         Campbell Barton <ideasman42@gmail.com>
;; URL: https://github.com/ideasman42/emacs-inkpot-theme
;; Package-Version: 20181119.706
;; Version: 0.1
;; Keywords: color, theme

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;; This file is not part of Emacs.

;;; Commentary:

;; This file is based on Per Vognsen's port of the original vim theme.
;; The original emacs color-theme version is found at http://www.emacswiki.org/emacs/ColorThemeInkpot.

;;; Code:

;; Colors from original Vim theme (for reference)
;; as of https://github.com/ciaranm/inkpot (Feb 11, 2013)
;; #000000 #00ff8b #1e1e27 #2e2e2e #2e2e37 #2e2e3f
;; #303030 #306b8f #306d30 #3b205d #3e3e5e #404040
;; #409090 #4a2a4a #4b208f #4e4e8f #66cccc #6d3030
;; #6e2e2e #6e6eaf #7070a0 #7e7eae #808bed #8b8bcd
;; #8b8bff #8fff8b #ad600b #ad7b20 #ad7b57 #af4f4b
;; #b9b9b9 #c080d0 #cc6666 #cc66cc #cccc66 #cd8b00
;; #cd8b60 #ce4e4e #ce8e4e #cfbfad #cfcfcd #d0a060
;; #df9f2d #eeeeee #f0ad6d #fdab60 #fdd090 #ff8bff
;; #ffcd00 #ffcd8b #ffffcd #ffffff


(deftheme inkpot "Dark color scheme with bright easily identifiable colors.")

(custom-theme-set-faces
 'inkpot
 '(default ((t (:background "#1e1e27" :foreground "#cfbfad"))))
 '(font-lock-builtin-face ((t (:foreground "#cfbfad"))))
 '(region ((t (:background "#4e4e8f" :foreground "#ffffff"))))
 '(secondary-selection ((t (:foreground "#b38363" :inverse-video t)))) ; match gvim secondary selection
 '(highlight ((t (:background "#404040"))))
 '(hl-line ((t (:background "#2e2e37"))))
 '(fringe ((t (:background "#2e2e2e" :foreground "#8b8bcd"))))
 ;; old 'linum' mode,
 '(linum ((t (:background "#2e2e2e" :foreground "#8b8bcd"))))
 ;; emacs 26+ 'display-line-numbers-mode'
 '(line-number ((t (:background "#2e2e2e" :foreground "#8b8bcd"))))
 '(line-number-current-line ((t (:bold t :background "#1e1e27" :foreground "#ffcd00"))))
 '(cursor ((t (:background "#8b8bff" :foreground "#cfdfef"))))
 '(show-paren-match ((t (:background "#4e4e8f"))))
 '(show-paren-match-face ((t (:background "#4e4e8f"))))
 '(link ((t (:foreground "#ff8bff"))))
 '(link-visited ((t (:foreground "#cb6ecbv"))))  ; not a vim color, just a little darker

 ;; white-space
 '(whitespace-trailing ((nil (:background "#343443" :foreground nil))))
 '(whitespace-space ((nil (:background nil :foreground "#434357"))))
 '(whitespace-tab ((nil (:background nil :foreground "#434357"))))

 ; additions that fit in with the general theme
 '(button ((t (:underline t :foreground "#ff8bff"))))
 '(escape-glyph ((t (:foreground "#8b8bcd"))))   ; not matching gvim, just nice color

 ; isearch
 ;
 ; note: original theme doesn't show different colors here,
 ; simply use bold for 'isearch'.
 '(isearch ((t (:bold t :foreground "#303030" :background "#ad7b57"))))
 '(lazy-highlight ((t  (:foreground "#303030" :background "#ad7b57"))))
 '(isearch-fail ((t (:foreground "#ffffff" :background "#ce4e4e"))))
 '(mode-line ((t (:bold t :foreground "#b9b9b9" :background "#3e3e5e"
                   :box (:line-width -1 :color "#7070a0")))))
 '(mode-line-inactive ((t (:bold t :foreground "#708090" :background "#3e3e5e"
                            :box (:line-width -1 :color "#7070a0")))))
 '(mode-line-buffer-id ((t (:bold t :foreground "#b9b9b9" :background "#3e3e5e"))))
 '(minibuffer-prompt ((t (:bold t :foreground "#708090"))))
 '(default-italic ((t (:italic t))))
 '(font-lock-builtin-face ((t (:foreground "#ff8bff"))))
 '(font-lock-comment-face ((t (:foreground "#cd8b00"))))
 '(font-lock-comment-delimiter-face ((t (:foreground "#cd8b00"))))
                                        ;'(font-lock-doc-face ((t (:foreground "#c080d0"))))
 '(font-lock-doc-face ((t (:foreground "#808bed"))))  ; alternate comment face
 '(font-lock-constant-face ((t (:foreground "#409090"))))
 '(font-lock-function-name-face ((t (:foreground "#c080d0"))))
 '(font-lock-keyword-face ((t (:foreground "#808bed"))))
 '(font-lock-preprocessor-face ((t (:foreground "#409090"))))
 '(font-lock-reference-face ((t (:bold t :foreground "#808bed"))))
 '(font-lock-string-face ((t (:foreground "#ffcd8b" :background "#404040"))))
 '(font-lock-type-face ((t (:foreground "#ff8bff"))))
 '(font-lock-variable-name-face ((t nil)))
 '(font-lock-warning-face ((t (:foreground "#ffffff" :background "#6e2e2e"))))
 '(w3m-anchor ((t (:foreground "#c080d0"))))
 '(info-xref ((t (:foreground "#409090"))))
 '(info-menu-star ((t (:foreground "#409090"))))
 '(message-cited-text ((t (:foreground "#cd8b00"))))
 '(gnus-cite-face-1 ((t (:foreground "#708090"))))
 '(gnus-cite-face-2 ((t (:foreground "#df9f2d"))))
 '(gnus-cite-face-3 ((t (:foreground "#ad7fa8"))))
 '(gnus-cite-face-4 ((t (:foreground "#4090904"))))
 '(gnus-group-mail-1-empty-face ((t (:foreground "#c080d0"))))
 '(gnus-group-mail-1-face ((t (:bold t :foreground "#c080d0"))))
 '(gnus-group-mail-2-empty-face ((t (:foreground "#409090"))))
 '(gnus-group-mail-2-face ((t (:bold t :foreground "#409090"))))
 '(gnus-group-mail-3-empty-face ((t (:foreground "#506dbd"))))
 '(gnus-group-mail-3-face ((t (:bold t :foreground "#cd8b00"))))
 '(gnus-group-mail-3 ((t (:bold t :foreground "#cd8b00"))))
 '(gnus-group-mail-low-empty-face ((t (:foreground "#8b8bcd"))))
 '(gnus-group-mail-low-face ((t (:bold t :foreground "8b8bcd"))))
 '(gnus-group-news-1-empty-face ((t (:foreground "#c080d0"))))
 '(gnus-group-news-1-face ((t (:bold t :foreground "#c080d0"))))
 '(gnus-group-news-2-empty-face ((t (:foreground "#409090"))))
 '(gnus-group-news-2-face ((t (:bold t :foreground "#409090"))))
 '(gnus-group-news-3-empty-face ((t (:foreground "#506dbd"))))
 '(gnus-group-news-3-empty ((t (:foreground "#506dbd"))))
 '(gnus-group-news-3-face ((t (:bold t :foreground "#cd8b00"))))
 '(gnus-group-news-low-empty-face ((t (:foreground "8b8bcd"))))
 '(gnus-group-news-low-face ((t (:bold t :foreground "8b8bcd"))))
 '(gnus-header-name-face ((t (:bold t :foreground "#ab60ed"))))
 '(gnus-header-from ((t (:bold t :foreground "#cd8b00"))))
 '(gnus-header-subject ((t (:foreground "#808bed"))))
 '(gnus-header-content ((t (:italic t :foreground "#409090"))))
 '(gnus-header-newsgroups-face ((t (:italic t :bold t :foreground "#ff8bff"))))
 '(gnus-signature-face ((t (:italic t :foreground "#708090"))))
 '(gnus-summary-cancelled-face ((t (:foreground "#cd8b00"))))
 '(gnus-summary-cancelled ((t (:foreground "#cd8b00"))))
 '(gnus-summary-high-ancient-face ((t (:bold t :foreground "#ab60ed"))))
 '(gnus-summary-high-read-face ((t (:bold t :foreground "#c080d0"))))
 '(gnus-summary-high-ticked-face ((t (:bold t :foreground "#af4f4b"))))
 '(gnus-summary-high-unread-face ((t (:bold t :foreground "#ffcd8b"))))
 '(gnus-summary-low-ancient-face ((t (:italic t :foreground "#c080d0"))))
 '(gnus-summary-low-read-face ((t (:italic t :foreground "#ab60ed"))))
 '(gnus-summary-low-ticked-face ((t (:italic t :foreground "#af4f4b"))))
 '(gnus-summary-low-unread-face ((t (:italic t :foreground "#ffcd8b"))))
 '(gnus-summary-normal-ancient-face ((t (:foreground "#8b8bcd"))))
 '(gnus-summary-normal-read-face ((t (:foreground "#2e3436"))))
 '(gnus-summary-normal-read ((t (:foreground "#2e3436"))))
 '(gnus-summary-normal-ticked-face ((t (:foreground "#af4f4b"))))
 '(gnus-summary-normal-unread-face ((t (:foreground "#ffcd8b"))))
 '(gnus-summary-selected ((t (:background "#404040" :foreground "#ffcd8b"))))
 '(gnus-header-from ((t (:bold t :foreground "#cd8b00"))))
 '(message-header-name-face ((t (:foreground "#ab60ed"))))
 '(message-header-name ((t (:foreground "#ab60ed"))))
 '(message-header-newsgroups-face ((t (:italic t :bold t :foreground "#ff8bff"))))
 '(message-header-other-face ((t (:foreground "#409090"))))
 '(message-header-other ((t (:foreground "#409090"))))
 '(message-header-xheader-face ((t (:foreground "#409090"))))
 '(message-header-subject ((t (:foreground "#808bed"))))
 '(message-header-to ((t (:foreground "#cd8b00"))))
 '(message-header-cc ((t (:foreground "#409090"))))
 '(font-latex-bold-face ((t (:foreground "#cd8b00"))))
 '(font-latex-italic-face ((t (:foreground "#808bed" :italic t))))
 '(font-latex-string-face ((t (:foreground "#708090"))))
 '(font-latex-match-reference-keywords ((t (:foreground "#708090"))))
 '(font-latex-match-variable-keywords ((t (:foreground "#708090"))))

 ;; Haskell
 '(haskell-operator-face ((t (:foreground "#737dd5"))))

 ;; Org-Mode
 '(org-hide ((t (:foreground "#708090"))))
 '(org-level-1 ((t (:bold t :foreground "#8b8bcd" :height 1.0))))
 '(org-level-2 ((t (:bold nil :foreground "#409090" :height 1.0))))
 '(org-level-3 ((t (:bold t :foreground "#df9f2d" :height 1.0))))
 '(org-level-4 ((t (:bold nil :foreground "#af4f4b" :height 1.0))))
 '(org-date ((t (:underline t :foreground "#f0ad6d"))))
 '(org-footnote  ((t (:underline t :foreground "#ad600b"))))
 '(org-link ((t (:underline t :foreground "#708090" ))))
 '(org-special-keyword ((t (:foreground "#ad600b"))))
 '(org-verbatim ((t (:foreground "#ffcd8b" :background "#404040"))))
 '(org-code ((t (:foreground "#ffcd8b" :background "#404040"))))
 '(org-block ((t (:foreground "#708090"))))
 '(org-quote ((t (:inherit org-block :slant italic))))
 '(org-verse ((t (:inherit org-block :slant italic))))
 '(org-todo ((t (:bold t :foreground "#af4f4b"))))
 '(org-done ((t (:bold t :foreground "#409090"))))
 '(org-warning ((t (:underline t :foreground "#409090"))))
 '(org-agenda-structure ((t (:weight bold :foreground "#af4f4b"))))
 '(org-agenda-date ((t (:foreground "#409090"))))
 '(org-agenda-date-weekend ((t (:weight normal :foreground "8b8bcd"))))
 '(org-agenda-date-today ((t (:weight bold :foreground "#cd8b00"))))

 ;; reStructuredText
 '(rst-external ((t (:foreground "#ff8bff"))))
 '(rst-definition ((t (:foreground "#409090"))))
 '(rst-directive ((t (:foreground "#808bed"))))
 '(rst-emphasis1 ((t (:italic t))))
 '(rst-emphasis2 ((t (:weight bold t))))
 '(rst-reference ((t (:foreground "#ff8bff"))))
 ;; titles baseline
 '(rst-adornment ((t (:foreground "#af4f4b"))))
 ;; titles
 '(rst-level-1 ((t (:foreground "#af4f4b"))))
 '(rst-level-2 ((t (:foreground "#af4f4b"))))
 '(rst-level-3 ((t (:foreground "#af4f4b"))))
 '(rst-level-4 ((t (:foreground "#af4f4b"))))
 '(rst-level-5 ((t (:foreground "#af4f4b"))))
 '(rst-level-6 ((t (:foreground "#af4f4b"))))

 ;; Colors for popular plugins

 ;; highlight-numbers (melpa)
 '(highlight-numbers-number ((t (:foreground "#f0ad6d"))))

 ;; auto-complete (melpa)
 '(ac-candidate-face ((t (:foreground "#ffffff" :background "#4e4e8f"))))
 '(ac-selection-face ((t (:foreground "#ffffff" :background "#2e2e3f" :weight bold))))

 ;; ivy (melpa)
 '(ivy-current-match ((t (:background "#4e4e8f" :foreground "#ffffff"))))
 ;; highlight matching chars (same as isearch)
 '(ivy-minibuffer-match-face-2 ((t (:background "#ad7b57" :foreground "#303030"))))

 ;; company (melpa)
 '(company-tooltip ((t (:background "#4e4e8f" :foreground "#ffffff"))))
 '(company-tooltip-selection ((t (:background "#2e2e3f" :weight bold))))
 '(company-tooltip-annotation ((t (:foreground "#cfbfad"))))
 '(company-tooltip-common ((t (:foreground "#303030" :background "#ad7b57"))))

 '(company-scrollbar-bg ((t (:background "#6e6eaf"))))
 ;; not based on original theme, could change
 '(company-scrollbar-fg ((t (:background "#000000"))))

 ;; helm (melpa)
 '(helm-selection ((t (:background "#2e2e37"))))

 ;; neotree
 '(neo-banner-face ((t (:foreground "#cd8b00"))))
 '(neo-header-face ((t (:foreground "#cd8b00"))))
 '(neo-root-dir-face ((t (:foreground "#808bed"))))
 '(neo-dir-link-face ((t (:foreground "#00ff8b"))))
 '(neo-expand-btn-face ((t (:foreground "#808bed"))))
 '(neo-file-link-face ((t (:foreground "#cfbfad"))))

 ;; highlight-indent-guides (melpa)
 '(highlight-indent-guides-odd-face ((t (:background "#252530"))))
 '(highlight-indent-guides-even-face ((t (:background "#1b1b24"))))

 ;; visual-indentation-mode (stand alone package)
 '(visual-indentation-light-face ((t (:background "#343443"))))
 '(visual-indentation-dark-face ((t (:background "#252530"))))
)
;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'inkpot)

;;; inkpot-theme.el ends here
