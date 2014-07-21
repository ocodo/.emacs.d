;;; midnight-theme.el - GNU Emacs 24
;;
;; Originally by Gordon Messmer (2001-02-07)
;; Ported to Emacs 24 by Joe Winder (2012-08-07)
;;
;; https://github.com/jwinder/midnight-theme.el
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

(deftheme midnight
  "midnight color theme")

(custom-theme-set-faces
 'midnight
 '(default ((t (:background "black" :foreground "grey85"))))
 '(cursor ((t (:foreground "grey85"))))
 '(region ((t (:background "grey15"))))
 '(fringe ((t (:background "#252323"))))
 '(border-color ((t (:background "black"))))
 '(cursor-color ((t (:background "grey85"))))
 '(highlight-current-line-face ((t (:background "grey12"))))
 '(hl-line ((t (:background "#252323"))))
 '(font-lock-builtin-face ((t (:foreground "SkyBlue"))))
 '(font-lock-comment-face ((t (:italic t :foreground "grey60"))))
 '(font-lock-comment-delimiter-face ((t (:forground "grey60"))))
 '(font-lock-constant-face ((t (:forground "#1AB0B0"))))
 '(font-lock-function-name-face ((t (:foreground "SlateBlue"))))
 '(font-lock-keyword-face ((t (:foreground "Cyan"))))
 '(font-lock-string-face ((t (:foreground "#F261F2"))))
 '(font-lock-warning-face ((t (:bold t :foreground "Pink"))))
 '(font-lock-variable-name-face ((t (:foreground "DarkGoldenrod"))))
 '(font-lock-type-face ((t (:foreground "#008080")))) ;; dark cyan
 '(paren-face-match-light ((t (:background "grey30"))))
 '(highlight ((t (:background "blue"))))
 '(modeline ((t (:background "#3F3B3B" :foreground "white"))))
 '(modeline-buffer-id ((t (:background "#3F3B3B" :foreground "white"))))
 '(modeline-mousable ((t (:background "#a5baf1" :foreground "black"))))
 '(modeline-mousable-minor-mode ((t (:background "#a5baf1" :foreground "#000000"))))
 '(primary-selection ((t (:background "#3B3B3F"))))
 '(isearch ((t (:background "#555555"))))
 '(zmacs-region ((t (:background "#555577"))))
 '(secondary-selection ((t (:background "#545459"))))
 '(flymake-errline ((t (:background "LightSalmon" :foreground "#000000"))))
 '(flymake-warnline ((t (:background "LightSteelBlue" :foreground "#000000"))))
 '(underline ((t (:underline t))))
 '(minibuffer-prompt ((t (:bold t :foreground "SkyBlue"))))
 '(italic ((t (:italic)))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'midnight)
