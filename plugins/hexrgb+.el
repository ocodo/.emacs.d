;;; package --- colour-plus+ - Additional Color functions and helpers for hexrgb.el

;;; Commentary:
;;  Set hue, sat, brightness of a hexrgb color + a bunch of other
;;  color tools I built for personal use.
;;
;;  The color stepping (dark / light; sat / desat; hue up/down) is particularly nice...
;;  Always use in conjunction with rainbow-mode
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

(defcustom hexrgb+-color-group-format
  "%s, "
  "Used by the hexrgb-hex-(hue|sat|val)-group functions."
  :type 'string
  :group 'hexrgb+)

(defcustom  hexrgb+-color-adjust-brightness-step
  5
  "Amount (%) to step adjust color brightness."
  :type 'integer
  :group 'hexrgb+)

(defcustom hexrgb+-color-adjust-saturation-step
  5
  "Amount (%) to step adjust color saturation."
  :type 'integer
  :group 'hexrgb+)

(defcustom hexrgb+-color-adjust-hue-step
  5
  "Amount (°) to step adjust color hue."
  :type 'integer
  :group 'hexrgb+)

(defun hexrgb+-hex-to-rgb (hex)
  "Convert a 6 digit HEX color to r g b."
  (setq hex (replace-regexp-in-string "#" "" hex))
  (mapcar #'(lambda (s) (/ (string-to-number s 16) 255.0))
          (list (substring hex 0 2)
                (substring hex 2 4)
                (substring hex 4 6))))

(defun hexrgb+-hex-to-hsv (hex)
  "Convert a 6 digit HEX color to h s v."
  (hexrgb+-rgb-to-hsv (hexrgb+-hex-to-rgb hex)))

(defun hexrgb+-hsv-to-hex (h s v)
  "Convert H S V to a 6 digit HEX color."
  (hexrgb+-rgb-to-hex (hexrgb+-hsv-to-rgb h s v)))

(defun hexrgb+-rgb-to-hex (rgb)
  "Replacement simple RGB to hex."
  (destructuring-bind
      (red green blue)
      (mapcar 'dec-to-byte   rgb)
    (format "#%02X%02X%02X" red green blue)))

(defun hexrgb+-rgb-to-hsv (rgb)
  "Convert RGB, a list of (r g b) to list (h s v).
For this module, h is returned as [0-1] instead of [0-360]."
  (destructuring-bind
      (red green blue) rgb
    (let*
        ((val (max red green blue))
         (delta (- val (min red green blue)))
         (sat (if (plusp val)
                  (/ delta val)
                0))
         (normalize #'(lambda
                        (constant right left)
                        (let ((hue (+ constant (/ (* 60 (- right left)) delta))))
                          (if (minusp hue)
                              (+ hue 360)
                            hue)))))
      (list (/ (cond
                ((zerop sat) 0)
                ((= red val) (funcall normalize 0 green blue)) ; dominant red
                ((= green val) (funcall normalize 120 blue red)) ; dominant green
                (t (funcall normalize 240 red green)))
               360.0)
            sat
            val))))

(defun hexrgb+-hsv-to-rgb (h s v)
  "Convert hsv (H S V) to red green blue.
For this module, H is exepected as [0-1] instead of [0-360]."
  (let* ((i (floor (* h 6.0)))
         (f (- (* h 6.0) i))
         (p (* v (- 1.0 s)))
         (q (* v (- 1.0 (* f s))))
         (d (* v (- 1.0 (* (- 1.0 f) s))))
         (m (% i 6)))
    (cond
     ((= m 0) (list v d p))
     ((= m 1) (list q v p))
     ((= m 2) (list p v d))
     ((= m 3) (list p q v))
     ((= m 4) (list d p v))
     ((= m 5) (list v p q)))))

(defun hexrgb+-replace-current (fn &rest args)
  "Get the current unspaced string at point.
Replace with the return value of the function FN with ARGS"
  (let (pos1 pos2 len replacement excerpt change)
    (if (and transient-mark-mode mark-active)
        (setq pos1 (region-beginning) pos2 (region-end))
      (progn
        (when (looking-at "#") (forward-char 1))
        (setq pos1 (car (bounds-of-thing-at-point 'symbol))
              pos2 (cdr (bounds-of-thing-at-point 'symbol)))
        (when (> pos1 0)
          (setq pos1 (- pos1 1)))))
    (setq excerpt (buffer-substring-no-properties pos1 pos2))
    (when args (setq change (car args)))
    (setq replacement (funcall fn excerpt change))
    (delete-region pos1 pos2)
    (insert replacement)))

(defun hexrgb+-adjust-brightness (hex amount)
  "Adjust the HEX color brightness by AMOUNT 0.0-0.1."
  (destructuring-bind (hue sat val) (hexrgb+-hex-to-hsv hex)
    (setq val (min 1.0 (+ amount val)))
    (hexrgb+-rgb-to-hex
     (hexrgb+-hsv-to-rgb hue sat val))))

(defun hexrgb+-adjust-saturation (hex amount)
  "Adjust the HEX color saturation by AMOUNT 0.0-0.1."
  (destructuring-bind (hue sat val) (hexrgb+-hex-to-hsv hex)
    (setq sat (min 1.0 (+ sat amount)))
    (hexrgb+-rgb-to-hex
     (hexrgb+-hsv-to-rgb hue sat val))))

(defun hexrgb+-adjust-hue (hex amount)
  "Adjust the HEX color hue by AMOUNT 0.0-0.1."
  (destructuring-bind (hue sat val) (hexrgb+-hex-to-hsv hex)
    (setq hue (mod (+ hue amount) 1.0))
    (hexrgb+-rgb-to-hex
     (hexrgb+-hsv-to-rgb hue sat val))))

(defun hexrgb+-increase-brightness-by-step (x)
  "Increase brightness on hex color at point (or in region) by step.
Accepts universal argument (X)."
  (interactive "P")
  (unless (numberp x) (setq x 1))
  (hexrgb+-replace-current
   'hexrgb+-adjust-brightness
   (/ (* x hexrgb+-color-adjust-brightness-step) 100.0)))

(defun hexrgb+-decrease-brightness-by-step (x)
  "Decrease brightness on hex color at point (or in region) by step.
Accepts universal argument (X)."
  (interactive "P")
  (unless (numberp x) (setq x 1))
  (hexrgb+-replace-current
   'hexrgb+-adjust-brightness
   (/ (* (* -1 x) hexrgb+-color-adjust-brightness-step) 100.0)))

(defun hexrgb+-increase-saturation-by-step (x)
  "Increase saturation on hex color at point (or in region) by step.
Accepts universal argument (X)."
  (interactive "P")
  (unless (numberp x) (setq x 1))
  (hexrgb+-replace-current
   'hexrgb+-adjust-saturation
   (/ (* x hexrgb+-color-adjust-saturation-step) 100.0)))

(defun hexrgb+-decrease-saturation-by-step (x)
  "Decrease saturation on hex color at point (or in region) by step.
Accepts universal argument (X)."
  (interactive "P")
  (unless (numberp x) (setq x 1))
  (hexrgb+-replace-current
   'hexrgb+-adjust-saturation
   (/ (*  (* -1 x) hexrgb+-color-adjust-saturation-step) 100.0)))

(defun hexrgb+-increase-hue-by-step (x)
  "Increase hue on hex color at point (or in region) by step.
Accepts universal argument (X)."
  (interactive "P")
  (unless (numberp x) (setq x 1))
  (hexrgb+-replace-current
   'hexrgb+-adjust-hue
   (/ (* x hexrgb+-color-adjust-hue-step) 360.0)))

(defun hexrgb+-decrease-hue-by-step (x)
  "Decrease hue on hex color at point (or in region) by step.
Accepts universal argument (X)."
  (interactive "P")
  (unless (numberp x) (setq x 1))
  (hexrgb+-replace-current
   'hexrgb+-adjust-hue
   (/ (* (* -1 x) hexrgb+-color-adjust-hue-step) 360.0)))

(defun hexrgb+-set-brightness (color brightness)
  "Interactively change a COLOR's BRIGHTNESS."
  (interactive (list
                (read-from-minibuffer "Hex Color (#000000 - #FFFFFF): ")
                (/ (string-to-number (read-from-minibuffer "Set Brightness (0% - 100%): ")) 100.0)))
  (insert (hexrgb+-hex-set-brightness color brightness)))

(defun hexrgb+-set-saturation (color saturation)
  "Interactively change a COLOR's SATURATION."
  (interactive (list
                (read-from-minibuffer "Hex Color (#000000 - #FFFFFF): ")
                (/ (string-to-number (read-from-minibuffer "Set Saturation (0% - 100%): ")) 100.0)))
  (insert (hexrgb+-hex-set-saturation color saturation)))

(defun hexrgb+-set-hue (color hue)
  "Interactively change a COLOR's HUE."
  (interactive (list
                (read-from-minibuffer "Hex Color (#000000 - #FFFFFF): ")
                (/ (string-to-number (read-from-minibuffer "Set Hue (0° - 360°): ")) 360.0)))
  (insert (hexrgb+-hex-set-hue color hue)))

(defun hexrgb+-hex-set-brightness (hex val)
  "Change a HEX color's brightness VAL, amount values from 0.0-1.0.
returns a 6 digit hex color."
  (destructuring-bind (hue sat skip) (hexrgb+-hex-to-hsv hex)
    (hexrgb+-rgb-to-hex (hexrgb+-hsv-to-rgb hue sat val))))

(defun hexrgb+-hex-set-saturation (hex sat)
  "Change a HEX color's saturation SAT, amount values from 0-1.
returns a 6 digit hex color."
  (destructuring-bind (hue skip val) (hexrgb+-hex-to-hsv hex)
    (hexrgb+-rgb-to-hex (hexrgb+-hsv-to-rgb hue sat val))))

(defun hexrgb+-hex-set-hue (hex hue)
  "Change a HEX color's HUE, amount values from 0-1.
returns a 6 digit hex color."
  (destructuring-bind (skip sat val) (hexrgb+-hex-to-hsv hex)
    (hexrgb+-rgb-to-hex (hexrgb+-hsv-to-rgb hue sat val))))

(defun hexrgb+-hex-get-brightness (hex)
  "Get the brightness of HEX color."
  (first (hexrgb+-hex-to-hsv hex)))

(defun hexrgb+-hex-get-saturation (hex)
  "Get the saturation of HEX color."
  (second (hexrgb+-hex-to-hsv hex)))

(defun hexrgb+-hex-get-hue (hex)
  "Get the hue of HEX color."
  (third (hexrgb+-hex-to-hsv hex)))

(defun hexrgb+-hex-hue-group (hex)
  "Given a HEX color.
Insert a list of hexcolors of different hue."
  (interactive "sHex color: ")
  (loop for n from 9 downto 1 do
        (insert
         (format (or hexrgb+-color-group-format "\n%s")
                 (hexrgb+-hex-set-hue hex (* n 0.1))))))

(defun hexrgb+-hex-sat-group (hex)
  "Given a HEX color.
Insert a list of hexcolors of different saturation (sat)."
  (interactive "sHex color: ")
  (loop for n from 9 downto 1 do
        (insert
         (format (or hexrgb+-color-group-format "\n%s")
                 (hexrgb+-hex-set-saturation hex (* n 0.1))))))

(defun hexrgb+-hex-val-group (hex)
  "Given a HEX color.
Insert a list of hexcolors of different brightness (val)."
  (interactive "sHex color: ")
  (loop for n from 9 downto 1 do
        (insert
         (format (or hexrgb+-color-group-format "\n%s")
                 (hexrgb+-hex-set-brightness hex (* n 0.1))))))

(defun hexrgb+-interpolate (color1 color2)
  "Interpolate two colors COLOR1 and COLOR2, to get their mixed color."
  (destructuring-bind (r g b)
      (mapcar #'(lambda (n) (* (/ n 2) 255.0))
              (cl-mapcar '+ (hexrgb+-hex-to-rgb color1)
                         (hexrgb+-hex-to-rgb color2)))
    (format "#%02X%02X%02X" r g b)))

(defun hexrgb+-cssrgb-to-hex (cssrgb)
  "Convert a CSSRGB (or rgba) color to hex (alpha value is ignored)."
  (let ((rgb (cdr
              (s-match
               (concat "rgba?(\s*"
                       "\\([0-9]\\{1,3\\}\\(?:\s*%\\)?\\)\s*,\s*"
                       "\\([0-9]\\{1,3\\}\\(?:\s*%\\)?\\)\s*,\s*"
                       "\\([0-9]\\{1,3\\}\\(?:\s*%\\)?\\)\s*)")
               cssrgb))))
    (destructuring-bind (r g b) (mapcar 'string-to-number rgb)
      (format "#%02X%02X%02X" r g b))))

(defun dec-to-byte (n)
  "Convert N (0.0-1.0) to 0-255."
  (* n 255.0))

(defun hexrgb+-hex-to-cssrgb (hex)
  "Convert a HEX rgb color to cssrgb."
  (destructuring-bind (r g b)
      (mapcar 'dec-to-byte (hexrgb+-hex-to-rgb hex))
    (format "rgb(%i, %i, %i)" r g b)))

(defun hexrgb+-hex-to-cssrgba (hex)
  "Convert a HEX rgb color to css rgba (only with 1.0 alpha)."
  (destructuring-bind (r g b)
      (mapcar 'dec-to-byte (hexrgb+-hex-to-rgb hex))
    (format "rgba(%i, %i, %i, 1.0)" r g b)))

(defun hexrgb+-cssrgb-at-point-or-region-to-hex ()
  "CSS rgb color at point or region to hex rgb."
  (interactive)
  (hexrgb+-replace-current 'hexrgb+-cssrgb-to-hex))

(defun hexrgb+-hexcolor-at-point-or-region-to-css-rgb ()
  "Hex rgb color at point or region to css rgb color."
  (interactive)
  (hexrgb+-replace-current 'hexrgb+-hex-to-cssrgb))

(defun hexrgb+-hexcolor-at-point-or-region-to-css-rgba ()
  "Hex rgb color at point or region to css rgba.
Opacity is always set to 1.0."
  (interactive)
  (hexrgb+-replace-current 'hexrgb+-hex-to-cssrgba))

;; ERT Tests...

(ert-deftest test-dec-to-byte ()
  "Test conversion of 0.0-0.1 to 0-255."
  (skip-unless (featurep 'hexrgb+))
  (should (equal (dec-to-byte 0)      0.0))
  (should (equal (dec-to-byte 1)      255.0))
  (should (equal (dec-to-byte 0.3231) 82.3905))
  (should (equal (dec-to-byte 0.7)    178.5))
  (should (equal (dec-to-byte 0.25)   63.75)))

(ert-deftest test-hexrgb+-interpolate ()
  "Test color interpolation."
  (skip-unless (featurep 'hexrgb+))
  (should (equal (hexrgb+-interpolate "#FFFFFF" "#000000") "#7F7F7F"))
  (should (equal (hexrgb+-interpolate "#0077FF" "#111111") "#084488"))
  (should (equal (hexrgb+-interpolate "#FF7700" "#111111") "#884408"))
  (should (equal (hexrgb+-interpolate "#7F7F7F" "#7F7F7F") "#7F7F7F"))
  (should (equal (hexrgb+-interpolate "#000000" "#000000") "#000000")))

(ert-deftest test-hexrgb+-hex-to-cssrgb ()
  "Test conversion of hex to rgb css."
  (skip-unless (featurep 'hexrgb+))
  (should (equal (hexrgb+-hex-to-cssrgb "#347291") "rgb(52, 114, 145)")))

(ert-deftest test-hexrgb+-hex-to-cssrgba ()
  "Test conversion of hex to rgb css."
  (skip-unless (featurep 'hexrgb+))
  (should (equal (hexrgb+-hex-to-cssrgba "#347291") "rgba(52, 114, 145, 1.0)")))

(ert-deftest test-hexrgb+-cssrgb-to-hex ()
  "Test conversion of css rgb to hex."
  (skip-unless (featurep 'hexrgb+))
  (should (equal (hexrgb+-cssrgb-to-hex "rgb(52, 114, 145)") "#347291"))
  (should (equal (hexrgb+-cssrgb-to-hex "rgb(10%, 20%, 90%)") "#0A145A"))
  ;; pending (should (equal (hexrgb+-cssrgb-to-hex "rgba(52, 114, 145, 1.0)") "#347291"))
  ;; pending (should (equal (hexrgb+-cssrgb-to-hex "rgba(10%, 20%, 90%, 1.0)") "#0A145A"))
  )

(ert-deftest test-hexrgb+-hex-set-hue ()
  "Test setting brightness of hex."
  (skip-unless (featurep 'hexrgb+))
  (should (equal (hexrgb+-hex-set-hue "#FF7700" 0.5) "#00FFFF")))

(ert-deftest test-hexrgb+-hex-set-sat ()
  "Test setting sat of hex."
  (skip-unless (featurep 'hexrgb+))
  (should (equal (hexrgb+-hex-set-saturation "#FF7700" 0.5) "#FFBB7F")))

(ert-deftest test-hexrgb+-hex-set-brightness ()
  "Test setting hue of hex."
  (skip-unless (featurep 'hexrgb+))
  (should (equal (hexrgb+-hex-set-brightness "#FF7700" 0.5) "#7F3B00")))

(ert-deftest test-hexrgb+-hex-to-rgb ()
  "Test conversion of hex to rgb."
  (skip-unless (featurep 'hexrgb+))
  (should (equal (hexrgb+-hex-to-rgb "#347291")
                 (list
                  0.20392156862745098
                  0.4470588235294118
                  0.5686274509803921))))

(ert-deftest test-hexrgb+-hex-to-hsv ()
  "Test conversion of hex to hsv."
  (skip-unless (featurep 'hexrgb+))
  (should (equal (hexrgb+-hex-to-hsv "#347291")
                 (list
                  0.5555555555555556
                  0.6413793103448275
                  0.5686274509803921))))

(ert-deftest test-hexrgb+-hsv-to-hex ()
  "Test conversion of hsv hex."
  (skip-unless (featurep 'hexrgb+))
  (should (equal (hexrgb+-hsv-to-hex
                  0.5555555555555556
                  0.6413793103448275
                  0.5686274509803921)
                 "#347191")))

(ert-deftest test-hexrgb+-rgb-to-hex ()
  "Test conversion of rgb to hex."
  (skip-unless (featurep 'hexrgb+))
  (should (equal (hexrgb+-rgb-to-hex
                  (list 0.20392156862745098
                        0.4470588235294118
                        0.5686274509803921))
                 "#347291"
                 )))

(ert-deftest test-hexrgb+-rgb-to-hsv ()
  "Test conversion of rgb to hsv."
  (skip-unless (featurep 'hexrgb+))
  (should (equal (hexrgb+-hex-to-hsv "#347291")
                 (list 0.5555555555555556
                       0.6413793103448275
                       0.5686274509803921))))

(ert-deftest test-hexrgb+-hsv-to-rgb ()
  "Test conversion of hsv to rgb."
  (skip-unless (featurep 'hexrgb+))
  (should (equal (hexrgb+-hsv-to-rgb
                  0.5555555555555556 0.6413793103448275 0.5686274509803921)
                 (list 0.203921568627451 0.4470588235294117 0.5686274509803921))))

(ert-deftest test-hexrgb+-hex-get-brightness ()
  "Test getting brightness from hex."
  (skip-unless (featurep 'hexrgb+))
  (should (equal (hexrgb+-hex-get-brightness "#006091") 0.5563218390804597)))

(ert-deftest test-hexrgb+-hex-get-saturation ()
  "Test getting saturation from hex."
  (skip-unless (featurep 'hexrgb+))
  (should (equal (hexrgb+-hex-get-saturation "#006091") 1.0)))

(ert-deftest test-hexrgb+-hex-get-hue ()
  "Test getting hue from hex."
  (skip-unless (featurep 'hexrgb+))
  (should (equal (hexrgb+-hex-get-hue "#006091") 0.5686274509803921)))

(ert-deftest test-hexrgb+-adjust-sat ()
  "Test adjustment of sat (saturation)."
  (skip-unless (featurep 'hexrgb+))
  (should (equal (hexrgb+-adjust-saturation "#006091" -0.1) "#0E6491"))
  (should (equal (hexrgb+-adjust-saturation "#006091" -0.2) "#1C6991"))
  (should (equal (hexrgb+-adjust-saturation "#006091" -0.5) "#487891"))
  (should (equal (hexrgb+-adjust-saturation "#006091" -0.6) "#567D91"))
  (should (equal (hexrgb+-adjust-saturation "#006091" -0.7) "#658291"))
  (should (equal (hexrgb+-adjust-saturation "#006091" -0.8) "#748791"))
  (should (equal (hexrgb+-adjust-saturation "#006091" -0.9) "#828C91"))
  (should (equal (hexrgb+-adjust-saturation "#006091" -1.0) "#919191"))
  (should (equal (hexrgb+-adjust-saturation "#347291"  0.1) "#256D91"))
  (should (equal (hexrgb+-adjust-saturation "#347291"  0.2) "#176891"))
  (should (equal (hexrgb+-adjust-saturation "#256D91"  0.1) "#166891"))
  (should (equal (hexrgb+-adjust-saturation "#166891"  1.0) "#006091")))

(ert-deftest test-hexrgb+-adjust-val ()
  "Test adjustment of val (brightness)."
  (skip-unless (featurep 'hexrgb+))
  (should (equal (hexrgb+-adjust-brightness "#FFFFFF" -0.1) "#E5E5E5"))
  (should (equal (hexrgb+-adjust-brightness "#FFFFFF" -0.2) "#CCCCCC"))
  (should (equal (hexrgb+-adjust-brightness "#FFFFFF" -0.5) "#7F7F7F"))
  (should (equal (hexrgb+-adjust-brightness "#FFFFFF" -0.6) "#666666"))
  (should (equal (hexrgb+-adjust-brightness "#FFFFFF" -0.7) "#4C4C4C"))
  (should (equal (hexrgb+-adjust-brightness "#FFFFFF" -0.8) "#323232"))
  (should (equal (hexrgb+-adjust-brightness "#FFFFFF" -0.9) "#191919"))
  (should (equal (hexrgb+-adjust-brightness "#FFFFFF" -1.0) "#000000"))
  (should (equal (hexrgb+-adjust-brightness "#000000"  0.1) "#191919"))
  (should (equal (hexrgb+-adjust-brightness "#000000"  0.2) "#333333"))
  (should (equal (hexrgb+-adjust-brightness "#AA6600"  0.1) "#C37500"))
  (should (equal (hexrgb+-adjust-brightness "#329847"  0.5) "#53FF77")))

(ert-deftest test-hexrgb+-adjust-hue ()
  "Test adjustment of hue."
  (skip-unless (featurep 'hexrgb+))
  (should (equal (hexrgb+-adjust-hue "#FF0000" -0.1) "#FF0098"))
  (should (equal (hexrgb+-adjust-hue "#FF7700" -0.2) "#FF00BB"))
  (should (equal (hexrgb+-adjust-hue "#FFFF00" -0.5) "#0000FF"))
  (should (equal (hexrgb+-adjust-hue "#FF00FF" -0.6) "#98FF00"))
  (should (equal (hexrgb+-adjust-hue "#00FFFF" -0.7) "#CC00FF"))
  (should (equal (hexrgb+-adjust-hue "#0000FF" -0.8) "#FF00CC"))
  (should (equal (hexrgb+-adjust-hue "#0077FF" -0.3) "#43FF00"))
  (should (equal (hexrgb+-adjust-hue "#224477" -0.4) "#667722"))
  (should (equal (hexrgb+-adjust-hue "#543322"  0.1) "#545122"))
  (should (equal (hexrgb+-adjust-hue "#0474F9"  0.2) "#B904F9"))
  (should (equal (hexrgb+-adjust-hue "#AA6600"  0.1) "#87AA00"))
  (should (equal (hexrgb+-adjust-hue "#329847"  0.5) "#983183")))

(provide 'hexrgb+)

;; Local Variables:
;; eval: (when (fboundp 'rainbow-mode) (rainbow-mode +1))
;; End:

;;; hexrgb+.el ends here
