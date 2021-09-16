;; base16-darkviolet-theme.el -- A base16 colorscheme

;;; Commentary:
;; Base16: (https://github.com/chriskempson/base16)

;;; Authors:
;; Scheme: ruler501 (https://github.com/ruler501/base16-darkviolet)
;; Template: Kaleb Elwert <belak@coded.io>

;;; Code:

(require 'base16-theme)

(defvar base16-darkviolet-colors
  '(:base00 "#000000"
    :base01 "#231a40"
    :base02 "#432d59"
    :base03 "#593380"
    :base04 "#00ff00"
    :base05 "#b08ae6"
    :base06 "#9045e6"
    :base07 "#a366ff"
    :base08 "#a82ee6"
    :base09 "#bb66cc"
    :base0A "#f29df2"
    :base0B "#4595e6"
    :base0C "#40dfff"
    :base0D "#4136d9"
    :base0E "#7e5ce6"
    :base0F "#a886bf")
  "All colors for Base16 Dark Violet are defined here.")

;; Define the theme
(deftheme base16-darkviolet)

;; Add all the faces to the theme
(base16-theme-define 'base16-darkviolet base16-darkviolet-colors)

;; Mark the theme as provided
(provide-theme 'base16-darkviolet)

(provide 'base16-darkviolet-theme)

;;; base16-darkviolet-theme.el ends here
