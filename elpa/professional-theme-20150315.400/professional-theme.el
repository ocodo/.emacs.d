;;; professional-theme.el --- Emacs port of Vim's professional theme
;;
;; Filename: professional-theme.el
;; Description: Emacs port of Vim's professional theme
;; Author: Juanjo Alvarez <juanjo@juanjoalvarez.net>
;; Created: Thu Sep 10 01:04:58 2013 (-0400)
;; Version: 20140914.1533
;; Package-Version: 20150315.400
;; X-Original-Version: 0.0.3
;; URL: https://github.com/juanjux/emacs-professional-theme
;; Keywords: theme, light, professional
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;  Port of Narayanan Lyer's Vim professional Theme.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:
(deftheme professional "port of Vim's professional theme")

(let* ((pro/bgyellow "#FFFFDD") 
       (pro/fg "#000000")
       (pro/blood "#993300") 
       (pro/blue "#0000C8")
       (pro/lightblue "#5180B3")
       (pro/darkgreen "#006600")
       (pro/darkred "#660000")
       (pro/darkpurple "#6600FF")
       (pro/green "#339900")
       (pro/olivegreen "#666633")
       (pro/orange "#FF8512")
       (pro/pastelgreen "#006666")
       (pro/pastelred "#FF8080")
       (pro/pastelred2 "#FF8088")
       (pro/pink "#C000C8")
       (pro/purple "#9933FF")
       (pro/red "#990000")
       (pro/white "#FFFFFF")
       (pro/brightred "#FF0000")
       (pro/darkgray "#757575")
       (pro/mediumgray "#C0C0C0")
       (pro/lightgray "#E8E8E8") 
       (pro/brightyellow "#FFFF00"))


  ;; Set faces
  (custom-theme-set-faces
   `professional
   `(default ((t (:foreground ,pro/fg :background ,pro/bgyellow))))
   `(cursor  ((t (:foreground ,pro/bgyellow :background ,pro/pastelgreen))))
   `(region  ((t (:background "grey"))))
   `(font-lock-builtin-face		((t (:foreground ,pro/blue))))
   `(font-lock-comment-face		((t (:foreground ,pro/lightblue))))
   `(font-lock-comment-delimiter-face	((t (:foreground ,pro/lightblue))))
   `(font-lock-function-name-face	((t (:foreground ,pro/darkpurple))))
   `(font-lock-keyword-face		((t (:foreground ,pro/blue))))
   `(font-lock-string-face		((t (:foreground ,pro/darkgreen))))
   `(font-lock-preprocessor-face	((t (:foreground ,pro/blood))))
   `(font-lock-type-face		((t (:foreground ,pro/darkred))))
   `(font-lock-constant-face		((t (:foreground ,pro/purple))))
   `(font-lock-warning-face		((t (:foreground "red" :bold t))))
   `(font-lock-variable-name-face	((t (:foreground ,pro/fg))))
   `(font-lock-doc-face			((t (:foreground ,pro/olivegreen))))

   ;; mode line
   `(linum ((t (:background ,pro/pastelgreen :foreground ,pro/bgyellow))))

   ;; hl-line
   `(hl-line ((t (:background ,pro/lightgray))))

   ;; indent-guide-face
   `(indent-guide-face ((t (:foreground ,pro/mediumgray))))

   ;; powerline
   `(powerline-active1          ((t (:foreground ,pro/bgyellow :background ,pro/fg))))
   `(powerline-active2          ((t (:foreground ,pro/bgyellow :background ,pro/pastelgreen))))
   `(powerline-inactive1        ((t (:foreground ,pro/bgyellow :background ,pro/fg))))
   `(powerline-inactive2        ((t (:foreground ,pro/bgyellow :background ,pro/darkgray))))
   `(powerline-evil-insert-face ((t (:foreground ,pro/bgyellow :background ,pro/brightred))))
   `(powerline-evil-normal-face ((t (:foreground ,pro/bgyellow :background ,pro/pastelgreen))))

   ;; search
   `(isearch		((t (:foreground ,pro/fg :background ,pro/brightyellow))))
   `(lazy-highlight	((t (:foreground ,pro/fg :background ,pro/orange))))

   `(compilation-error		((t (:foreground ,pro/brightred :bold t))))
   `(compilation-warning	((t (:foreground ,pro/orange :bold t))))
   `(compilation-info		((t (:foreground ,pro/green :bold t))))

   ;;show paren
   `(show-paren-match ((t (:foreground ,pro/fg :background ,pro/darkgray))))
   `(show-paren-mismatch ((t (:inherit error))))

   ;; elscreen
   `(elscreen-tab-other-screen-face ((t (:foreground ,pro/bgyellow :background ,pro/pastelgreen))))

   ;; error
   `(error ((t (:foreground "red"))))


   ;; helm
   `(helm-M-x-key			((t (:foreground ,pro/orange :underline nil))))
   `(helm-header			((t (:foreground ,pro/bgyellow :background ,pro/fg))))
   `(company-preview-common             ((t (:inherit font-lock-comment-face))))


   ;; trailing whitespace
   `(trailing-whitespace ((t (:background "white" :bold t)))))

  (custom-theme-set-variables
   'professional
   `(ansi-color-names-vector
     [,pro/fg ,pro/red ,pro/green ,pro/olivegreen ,pro/blue ,pro/purple ,pro/olivegreen ,pro/fg])))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

;;;###autoload
(defun professional-theme()
  "Apply the professional-theme."
  (interactive)
  (load-theme 'professional t))


(provide-theme 'professional)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; professional-theme.el ends here
