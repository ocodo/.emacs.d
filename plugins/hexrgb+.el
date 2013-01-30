(require 'hexrgb)

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; New functions
;;

(defun hexrgb-hex-set-brightness
  (hex brightness)
  "set brightness of a hex color, amount values from 0-1
   returns a 6 digit hex color"
  (let* ((hsv (hexrgb-hex-to-hsv hex))
         (h (first hsv)) (s (second hsv)) (v (third hsv)))
    (hexrgb-hsv-to-hex h s (* brightness v) 2)))

(defun hexrgb-hex-set-saturation
  (hex saturation)
  "set saturation of a hex color, amount values from 0-1
   returns a 6 digit hex color"
  (let* ((hsv (hexrgb-hex-to-hsv hex))
         (h (first hsv)) (s (second hsv)) (v (third hsv)))
    (hexrgb-hsv-to-hex h (* saturation s) v 2)))

(defun hexrgb-hex-set-hue
  (hex hue)
  "set hue of a hex color, amount values from 0-1
   returns a 6 digit hex color"
  (let* ((hsv (hexrgb-hex-to-hsv hex))
         (s (second hsv)) (v (third hsv)))
    (hexrgb-hsv-to-hex hue s v 2)))

(defcustom hexrgb-color-group-format "%s, " "DOCSTRING")

(defun hexrgb-hex-hue-group (hex ) "Return a list of hexcolors of different hues"
  (interactive "sHex color: ")
  (loop for n from 9 downto 1 do
        (insert
         (format (or hexrgb-color-group-format "\n%s") (hexrgb-hex-set-hue hex (* n 0.1))))))

(defun hexrgb-hex-sat-group (hex) "DOCSTRING"
  (interactive "sHex color: ")
  (loop for n from 9 downto 1 do
        (insert
         (format (or hexrgb-color-group-format "\n%s") (hexrgb-hex-set-saturation hex (* n 0.1))))))

(defun hexrgb-hex-val-group (hex) "Insert a set of colors with a range of brightness (val) "
  (interactive "sHex color: ")
  (loop for n from 9 downto 1 do
        (insert
         (format (or hexrgb-color-group-format "\n%s") (hexrgb-hex-set-brightness hex (* n 0.1))))))

(defun hexrgb-cssrgb-to-hex (cssrgb)
  "convert a css rgb color to hex"
  (let ((rgb 
  (cdr (s-match  "rgb(\s*\\([0-9]\\{1,3\\}\\(?:\s*%\\)?\\)\s*,\s*\\([0-9]\\{1,3\\}\\(?:\s*%\\)?\\)\s*,\s*\\([0-9]\\{1,3\\}\\(?:\s*%\\)?\\)\s*)" cssrgb))))
    (setq r (nth 0 rgb) 
          g (nth 1 rgb)
          b (nth 2 rgb))
    (format "#%02X%02X%02X"
            (string-to-int r)
            (string-to-int g)
            (string-to-int b))))

(defun hexrgb-hex-to-cssrgb (hex)
  "convert a hex rgb color to css rgb"
  (let ((rgb nil))
    (setq rgb (hexrgb-hex-to-rgb hex))
    (format "rgb(%i, %i, %i)"
           (* (nth 0 rgb) 65535.0)
           (* (nth 1 rgb) 65535.0)
           (* (nth 2 rgb) 65535.0))))

(defun cssrgb-at-point-or-region-to-hex ()
  "css rgb(0,0,0) at point or region to hex #000000"
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
  "hex #000000 at point or region to css rgb(0,0,0)"
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

(provide 'hexrgb+)
