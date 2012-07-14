(defun shades-of-hex (hex)
  "Generate a table of color shades for a given hex color, showing
10 percentage samples for lightness, saturation & hue"
  (interactive "sHex: ")
  (loop for a from 10.0 downto 0.0 do
        (let* ((hsv (hexrgb-hex-to-hsv hex)) 
               (f   (* a 0.1)))
          (insert (format ";; %s %s %s : %d%% \n"
                          (hexrgb-hsv-to-hex (first hsv) (second hsv) (* f (third hsv)) 2)
                          (hexrgb-hsv-to-hex (first hsv) (* f (second hsv)) (third hsv) 2)
                          (hexrgb-hsv-to-hex (* f (first hsv)) (second hsv) (third hsv) 2)
                          (* 100 f))))))

;; Rainbow mode will shed more light on this... 
;;  
;; (set-face-attribute font-lock-
;;  
;; #876421 #876421 #876421 :#0E92DD #0E92DD #0E92DD :#8600DD #8600DD #8600DD : 
;; #795A1E #87672C #875D21 :#0C84C6 #229ADD #0ED8DD :#7900C6 #8F16DD #2100DD : 
;; #6C501B #876A36 #875621 :#0B75B0 #37A1DD #0EDD9C :#6B00B0 #982CDD #0044DD : 
;; #5E4617 #876E40 #875021 :#09669A #4CA9DD #0EDD56 :#5E009A #A042DD #00AADD : 
;; #513C14 #87714A #874921 :#085884 #60B0DD #0EDD11 :#500084 #A958DD #00DDA9 : 
;; #433210 #877554 #874221 :#07496E #75B8DD #50DD0E :#43006E #B16EDD #00DD43 : 
;; #36280D #87795E #873C21 :#053A58 #8ABFDD #95DD0E :#350058 #BA84DD #22DD00 : 
;; #281E0A #877C68 #873521 :#042C42 #9EC6DD #DBDD0E :#280042 #C39ADD #88DD00 : 
;; #1B1406 #878072 #872F21 :#021D2C #B3CEDD #DD990E :#1A002C #CBB0DD #DDCB00 : 
;; #0D0A03 #87837C #872821 :#010E16 #C8D5DD #DD530E :#0D0016 #D4C6DD #DD6500 : 
;; #000000 #878787 #872121 :#000000 #DDDDDD #DD0E0E :#000000 #DDDDDD #DD0000 : 
;;  
;; #276231 #276231 #276231 : 100% 
;; #23582C #2C6235 #296227 : 90% 
;; #1F4E27 #32623A #366227 : 80% 
;; #1B4422 #38623F #436227 : 70% 
;; #173A1D #3E6244 #506227 : 60% 
;; #133118 #446249 #5D6227 : 50% 
;; #0F2713 #4A624E #625A27 : 40% 
;; #0B1D0E #506253 #624D27 : 30% 
;; #071309 #566258 #624027 : 20% 
;; #030904 #5C625D #623327 : 10% 
;; #000000 #626262 #622727 : 0% 
;;  
;; font-lock-comment-face
;; font-lock-comment-delimiter-face
;; font-lock-string-face
;; font-lock-doc-face
;; font-lock-keyword-face
;; font-lock-builtin-face
;; font-lock-function-name-face
;; font-lock-variable-name-face
;; font-lock-type-face
;; font-lock-constant-face
;; font-lock-warning-face
;; font-lock-negation-char-face
;; font-lock-preprocessor-face
;; font-lock-constant-face
;; font-lock-reference-face
;;  
;;  
;; comment, comment-delimiter, string, doc, keyword, builtin, function-name, variable-name, type, constant, warning, negation-char, preprocessor, constant, reference
