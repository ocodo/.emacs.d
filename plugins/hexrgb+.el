;;; package --- hexrgb+ - Additional Color functions and helpers for hexrgb.el

;;; NOT YET WORKING!!!

;;; Commentary:
;;  Set hue, sat, brightness of a hexrgb color + a bunch of other
;;  color tools I built for personal use.
;;
;;; Licence:
;;  GNU/GPL2
;;
;;; Code:

(require 'hexrgb)
(require 's)

(defgroup hexrgb+ nil
  "Hexrgb+ customizations."
  :group 'tools)

(defcustom hexrgb-color-group-format
  "%s, "
  "Used by the hexrgb-hex-(hue|sat|val)-group functions."
  :group 'hexrgb+)

(defcustom hexrgb-color-adjust-brightness-step
  "10%"
  "Amount to step adjust color brightness."
  :group 'hexrgb+)
(defcustom hexrgb-color-adjust-saturation-step
  "10%"
  "Amount to step adjust color saturation."
  :group 'hexrgb+)
(defcustom hexrgb-color-adjust-hue-step
  "10°"
  "Amount to step adjust color hue."
  :group 'hexrgb+)

;; TODO: increase brightness on hex color at point (or in region)
;; TODO: decrease brightness on hex color at point (or in region)

;; TODO: increase saturation on hex color at point (or in region)
;; TODO: decrease saturation on hex color at point (or in region)

;; TODO: increase hue on hex color at point (or in region)
;; TODO: decrease hue on hex color at point (or in region)

(defun hexrgb-set-brightness (color brightness)
  "Interactively change a COLOR's BRIGHTNESS."
  (interactive (list
                (read-from-minibuffer "Hex Color (#000000 - #FFFFFF): ")
                (/ (string-to-number (read-from-minibuffer "Set Brightness (0% - 100%): ")) 100.0)))
  (insert (hexrgb-hex-set-brightness color brightness)))

(defun hexrgb-set-saturation (color saturation)
  "Interactively change a COLOR's SATURATION."
  (interactive (list
                (read-from-minibuffer "Hex Color (#000000 - #FFFFFF): ")
                (/ (string-to-number (read-from-minibuffer "Set Saturation (0% - 100%): ")) 100.0)))
  (insert (hexrgb-hex-set-saturation color saturation)))

(defun hexrgb-set-hue (color hue)
  "Interactively change a COLOR's HUE."
  (interactive (list
                (read-from-minibuffer "Hex Color (#000000 - #FFFFFF): ")
                (/ (string-to-number (read-from-minibuffer "Set Hue (0° - 360°): ")) 360.0)))
  (insert (hexrgb-hex-set-hue color hue)))

(defun hexrgb-hex-set-brightness (hex brightness)
  "Change a HEX color's BRIGHTNESS, amount values from 0-1.
returns a 6 digit hex color."
  (let* ((hsv (hexrgb-hex-24bit-to-hsv hex))
         (h (first hsv))
         (s (second hsv))
         (v (third hsv)))
    (hexrgb-hsv-to-hex h s brightness 2)))

(defun hexrgb-hex-set-saturation (hex saturation)
  "Change a HEX color's SATURATION, amount values from 0-1.
returns a 6 digit hex color."
  (let* ((hsv (hexrgb-hex-24bit-to-hsv hex))
         (h (first hsv)) (s (second hsv)) (v (third hsv)))
    (hexrgb-hsv-to-hex h (* saturation s) v 2)))

(defun hexrgb-hex-set-hue (hex hue)
  "Change a HEX color's HUE, amount values from 0-1.
returns a 6 digit hex color."
  (let* ((hsv (hexrgb-hex-24bit-to-hsv hex))
         (s (second hsv)) (v (third hsv)))
    (hexrgb-hsv-to-hex hue s v 2)))

(defun hexrgb-hex-get-brightness (hex)
  "Get the brightness of HEX color."
  (first (hexrgb-hex-24bit-to-hsv hex)))

(defun hexrgb-hex-get-saturation (hex)
  "Get the saturation of HEX color."
  (second (hexrgb-hex-24bit-to-hsv hex)))

(defun hexrgb-hex-get-hue (hex)
  "Get the hue of HEX color."
  (third (hexrgb-hex-24bit-to-hsv hex)))

(defun hexrgb-hex-hue-group (hex)
  "Given a HEX color.
Insert a list of hexcolors of different hue."
  (interactive "sHex color: ")
  (loop for n from 9 downto 1 do
        (insert
         (format (or hexrgb-color-group-format "\n%s") (hexrgb-hex-set-hue hex (* n 0.1))))))

(defun hexrgb-hex-sat-group (hex)
  "Given a HEX color.
Insert a list of hexcolors of different saturation (sat)."
  (interactive "sHex color: ")
  (loop for n from 9 downto 1 do
        (insert
         (format (or hexrgb-color-group-format "\n%s") (hexrgb-hex-set-saturation hex (* n 0.1))))))

(defun hexrgb-hex-val-group (hex)
  "Given a HEX color.
Insert a list of hexcolors of different brightness (val)."
  (interactive "sHex color: ")
  (loop for n from 9 downto 1 do
        (insert
         (format (or hexrgb-color-group-format "\n%s") (hexrgb-hex-set-brightness hex (* n 0.1))))))

(defun hexrgb-interpolate (color1 color2)
  "Interpolate two colors COLOR1 and COLOR2, to get their mixed color."
  (let* ((rgb1 (hexrgb-hex-24bit-to-rgb color1))
         (rgb2 (hexrgb-hex-24bit-to-rgb color2))
         (red (/ (+ (nth 0 rgb1) (nth 0 rgb2)) 2))
         (grn (/ (+ (nth 1 rgb1) (nth 1 rgb2)) 2))
         (blu (/ (+ (nth 2 rgb1) (nth 2 rgb2)) 2)))
    (format "#%02X%02X%02X"
            (* red 65535.0)
            (* grn 65535.0)
            (* blu 65535.0))))

(defun hexrgb-cssrgb-to-hex (cssrgb)
  "Convert a CSSRGB (or rgba) color to hex (alpha value is ignored)."
  (let (r g b
          (rgb
           (cdr
            (s-match  "rgba?(\s*\\([0-9]\\{1,3\\}\\(?:\s*%\\)?\\)\s*,\s*\\([0-9]\\{1,3\\}\\(?:\s*%\\)?\\)\s*,\s*\\([0-9]\\{1,3\\}\\(?:\s*%\\)?\\)\s*)"
                      cssrgb))))
    (setq r (nth 0 rgb)
          g (nth 1 rgb)
          b (nth 2 rgb))
    (format "#%02X%02X%02X"
            (string-to-number r)
            (string-to-number g)
            (string-to-number b))))

(defun hexrgb-hex-to-cssrgb (hex)
  "Convert a HEX rgb color to cssrgb."
  (let ((rgb nil))
    (setq rgb (hexrgb-hex-24bit-to-rgb hex))
    (format "rgb(%i, %i, %i)"
           (* (nth 0 rgb) 65535.0)
           (* (nth 1 rgb) 65535.0)
           (* (nth 2 rgb) 65535.0))))

(defun hexrgb-hex-to-cssrgba (hex)
  "Convert a HEX rgb color to css rgba (only with 1.0 alpha)."
  (let ((rgb nil))
    (setq rgb (hexrgb-hex-24bit-to-rgb hex))
    (format "rgba(%i, %i, %i, 1.0)"
           (* (nth 0 rgb) 65535.0)
           (* (nth 1 rgb) 65535.0)
           (* (nth 2 rgb) 65535.0))))

(defun cssrgb-at-point-or-region-to-hex ()
  "CSS rgb color at point or region to hex rgb."
  (interactive)
  (let (pos1 pos2 meat)
    (if (and transient-mark-mode mark-active)
        (setq pos1 (region-beginning)
              pos2 (region-end))
      (setq pos1 (car (bounds-of-thing-at-point 'symbol))
            pos2 (cdr (bounds-of-thing-at-point 'symbol))))
    (setq meat (hexrgb-cssrgb-to-hex (buffer-substring-no-properties pos1 pos2)))
    (delete-region pos1 pos2)
    (insert  meat)))

(defun hexcolor-at-point-or-region-to-css-rgb ()
  "Hex rgb color at point or region to css rgb color."
  (interactive)
  (let (pos1 pos2 meat)
    (if (and transient-mark-mode mark-active)
        (setq pos1 (region-beginning)
              pos2 (region-end))
      (setq pos1 (car (bounds-of-thing-at-point 'symbol))
            pos2 (cdr (bounds-of-thing-at-point 'symbol))))
    (setq meat (hexrgb-hex-to-cssrgb (buffer-substring-no-properties pos1 pos2)))
    (delete-region pos1 pos2)
    (insert  meat)))

(defun hexcolor-at-point-or-region-to-css-rgba ()
  "Hex rgb color at point or region to css rgba.
Opacity is always set to 1.0."
  (interactive)
  (let (pos1 pos2 meat)
    (if (and transient-mark-mode mark-active)
        (setq pos1 (region-beginning)
              pos2 (region-end))
      (setq pos1 (car (bounds-of-thing-at-point 'symbol))
            pos2 (cdr (bounds-of-thing-at-point 'symbol))))
    (setq meat (hexrgb-hex-to-cssrgba (buffer-substring-no-properties pos1 pos2)))
    (delete-region pos1 pos2)
    (insert  meat)))

(defun hexrgb-24bit-to-48bit (hex)
  "Convert a standard 6 digit HEX color to a whacky 12 digit hex color."
  (let (rgb r g b)
    (setq rgb (replace-regexp-in-string "#" "" hex))
    (setq r (substring rgb 0 2))
    (setq g (substring rgb 2 4))
    (setq b (substring rgb 4 6))
    (format "#%s00%s00%s00" r g b)))

(defun hexrgb-hex-24bit-to-hsv (hex)
  "Convert HEX 24bit using Drew's hexrgb to hsv which expects 48bit hex colors."
  (hexrgb-hex-to-hsv (hexrgb-24bit-to-48bit hex)))

(defun hexrgb-hex-24bit-to-rgb (hex)
  "Convert HEX 24bit using Drew's hexrgb to rgb which expects 48bit hex colors."
  (hexrgb-hex-to-rgb (hexrgb-24bit-to-48bit hex)))

(provide 'hexrgb+)

;; Local Variables:
;; eval: (when (fboundp 'rainbow-mode) (rainbow-mode +1))
;; End:

;;; hexrgb+.el ends here
