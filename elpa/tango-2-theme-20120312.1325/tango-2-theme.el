;;; tango-2-theme.el --- Tango 2 color theme for GNU Emacs 24
;; Author: Nick Parker
;; Version: 1.0.0
;; 
;; Ported theme to Emacs 24 color theme Nick Parker <nickp@developernotes.com>
;; original from Will Farrington <wcfarrington@gmail.com>
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

(deftheme tango-2
  "Tango 2 color theme")

(custom-theme-set-faces
 'tango-2
 '(default ((t (:background "#121212" :foreground "#eeeeec"))))
 '(cursor ((t (:foreground "#888888"))))
 '(region ((t (:background "#555753"))))
 '(highlight ((t (:background "#444444"))))
 '(modeline ((t (:background "#2e3436" :foreground "#eeeeec"))))
 '(modeline-inactive ((t (:background "#111111" :foreground "#cccddd"))))
 '(fringe ((t (:background "#111111"))))
 '(minibuffer-prompt ((t (:foreground "#729fcf"))))
 '(font-lock-builtin-face ((t (:foreground "#729fcf"))))
 '(font-lock-comment-face ((t (:foreground "#888a85"))))
 '(font-lock-constant-face ((t (:foreground "#ad7fa8"))))
 '(font-lock-function-name-face ((t (:foreground "#729fcf"))))
 '(font-lock-keyword-face ((t (:foreground "#fcaf3e"))))
 '(font-lock-string-face ((t (:foreground "#73d216"))))
 '(font-lock-type-face ((t (:foreground "#c17d11"))))
 '(font-lock-variable-name-face ((t (:foreground "#fce94f"))))
 '(font-lock-warning-face ((t (:bold t :foreground "#cc0000"))))
 '(isearch ((t (:background "#f57900" :foreground "#2e3436"))))
 '(lazy-highlight ((t (:background "#e9b96e" :foreground "#2e3436"))))
 '(link ((t (:foreground "#729fcf"))))
 '(link-visited ((t (:foreground "#ad7fa8"))))

 '(flyspell-duplicate ((t (:foreground "#fcaf3e"))))
 '(flyspell-incorrect ((t (:foreground "#cc0000"))))

 '(org-date ((t (:foreground "LightSteelBlue" :underline t))))
 '(org-hide ((t (:foreground "#2e3436"))))
 '(org-todo ((t (:inherit font-lock-keyword-face :bold t))))
 '(org-level-1 ((t (:inherit font-lock-function-name-face))))
 '(org-level-2 ((t (:inherit font-lock-variable-name-face))))
 '(org-level-3 ((t (:inherit font-lock-keyword-face))))
 '(org-level-4 ((t (:inherit font-lock-string-face))))
 '(org-level-5 ((t (:inherit font-lock-constant-face))))

 '(comint-highlight-input ((t (:italic t :bold t))))
 '(comint-highlight-prompt ((t (:foreground "#8ae234"))))
 '(isearch ((t (:background "#f57900" :foreground "#2e3436"))))
 '(isearch-lazy-highlight-face ((t (:foreground "#2e3436" :background "#e9b96e"))))
 '(paren-face-match ((t (:inherit show-paren-match-face))))
 '(paren-face-match-light ((t (:inherit show-paren-match-face))))
 '(paren-face-mismatch ((t (:inherit show-paren-mismatch-face))))
 '(persp-selected-face ((t (:foreground "#729fcf"))))
 '(show-paren-match-face ((t (:background "#729fcf" :foreground "#eeeeec"))))
 '(show-paren-mismatch-face ((t (:background "#ad7fa8" :foreground "#2e3436")))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'tango-2)
;;; tango-2-theme.el ends here