;;; main-line.el --- modeline replacement forked from an early version of powerline.el
;;;
;;; Author: Jason Milkins
;;; Version: 1.2.8
;; Package-Version: 20151120.1806
;;; Keywords: statusline / modeline
;;; Url: https://github.com/jasonm23/emacs-mainline
;;; Package-Requires: ((cl-lib "0.5"))
;;; Changelog:
;;; 1.2.8 : Fixed percent-xpm indicator on Linux
;;; 1.2.7 : Added color interpolation to do VERY basic anti-aliasing on xpms, used on curved
;;;       : separators, contour, curve, rounded, roundstub, brace, wave
;;;       : (inc. left/right variants)
;;; 1.2.6 : fixed rounded xpm
;;; 1.2.5 : shrunk percent xpm to 3x14 - added roundstub, roundstub-left, roundstub-right
;;; 1.2.4 : added separator style, contour, contour-left & contour-right, resized brace to 19px
;;; 1.2.3 : added separator style, brace
;;; 1.2.2 : using force-mode-line-update instead of redraw-mode-line
;;; 1.2.1 : added customize group as child of mode-line
;;; 1.2.0 : renamed to main-line
;;;       : - main-line-percent-xpm used by default
;;;       : - default style set to wave
;;; 1.1.0 : Wave, zigzag and butt separators
;;;       : - Changed main-line-arrow-shape to main-line-separator-style
;;;       : - styles for wave       / zigzag       / butt
;;;       : - styles for wave-left  / zigzag-left  / butt-left
;;;       : - styles for wave-right / zigzag-right / butt-right
;;; 1.0.4 : Fixed custom vars
;;; 1.0.3 : Fixed usage instructions
;;; 1.0.2 : Added to marmalade - documentation updated, renamed to main-line.
;;; 1.0.1 : added additional xpm shape chamfer14, adjusted chamfer xpm.
;;;       : main-line-color1, main-line-color2, main-line-arrow-shape
;;;       : - are now custom variables, to make them available via customize.
;;;       : - they could be set from deftheme: custom-theme-set-variables
;;;       : - already, so Emacs24 color themes can set them.
;;;   1.0 : forked from powerline 0.0.1 - added additional xpm shapes.
;;;
;;; (forked from the original emacs port of vim powerline > Powerline.el Nicolas Rougier)
;;;
;;; Commentary:
;;;
;;; Note Milkbox/MELPA users this is v1.2.7
;;;
;;; This is a fork of powerline.el which I began while the original
;;; authorship of powerline was unknown,
;;;
;;; -- Using main-line.el.
;;;
;;; Add a require to .emacs / init.el
;;;
;;;     (require 'main-line)
;;;
;;; You can loop through the different separator styles by clicking on
;;; them (directly on the separator)
;;;
;;; Or customize it by setting the custom variable:
;;;
;;;     main-line-separator-style
;;;
;;; e.g.
;;;
;;;     (setq main-line-separator-style 'wave)
;;;
;;; possible values...
;;;
;;; - contour
;;; - contour-left
;;; - contour-right
;;; - roundstub
;;; - roundstub-left
;;; - roundstub-right
;;; - brace
;;; - wave
;;; - zigzag
;;; - butt
;;; - wave-left
;;; - zigzag-left
;;; - butt-left
;;; - wave-right
;;; - zigzag-right
;;; - butt-right
;;; - chamfer
;;; - chamfer14
;;; - rounded
;;; - arrow
;;; - arrow14
;;; - slant
;;; - slant-left
;;; - slant-right
;;; - curve
;;;
;;; To customize the modeline - simply override the value of mode-line-format,
;;; see the default at the end of the script, as an example.
;;;
;;; You can create your own modeline additions by using the defmain-line macro.
;;;
;;; for example,
;;;
;;; (defmain-line row "%4l")
;;;
;;; gives you main-line-row to use in mode-line-format
;;;
;;; Note. main-line-percent-xpm requires 18px separators (use
;;; main-line-percent with arrow14 or chamfer14)
;;;

(require 'cl-lib)

(defgroup main-line nil
  "Alternative mode line formatting with xpm-bitmap separators"
  :group 'mode-line)

(defcustom main-line-color1 "#123550"
  "Mainline color background 1"
  :group 'main-line)

(defcustom main-line-color2 "#112230"
  "Mainline color background 2"
  :group 'main-line)

(defcustom main-line-separator-style 'wave
  " Mainline separator stylename, which can be: wave, zigzag,
curve, wave-left, zigzag-left, curve-left, wave-right,
zigzag-right, curve-right, rounded, contour, contour-left,
contour-right, brace, chamfer, slant, slant-left, slant-right,
arrow, which are all 18px high, except for chamfer14 and arrow14 which
are both 14px high, and brace which is 19px high"
  :group 'main-line)

(defvar main-line-minor-modes nil)

(defvar main-line-buffer-size-suffix t
  "when set to true buffer size is shown as K/Mb/Gb etc.")

(defun interpolate (color1 color2)
  "Interpolate between two hex colors, they must be supplied as
  hex colors with leading # - Note: this has been implemented
  independently, there are functions in hexrgb.el that would help
  this out a bit, but I wanted this to require only cl-lib (which
  it built in), and nothing else."
  (let* (
         (c1 (replace-regexp-in-string "#" "" color1))
         (c2 (replace-regexp-in-string "#" "" color2)) 
         (c1r (string-to-number (substring c1 0 2) 16))
         (c1b (string-to-number (substring c1 2 4) 16))
         (c1g (string-to-number (substring c1 4 6) 16)) 
         (c2r (string-to-number (substring c2 0 2) 16))
         (c2b (string-to-number (substring c2 2 4) 16))
         (c2g (string-to-number (substring c2 4 6) 16)) 
         (red (/ (+ c1r c2r) 2))
         (grn (/ (+ c1g c2g) 2))
         (blu (/ (+ c1b c2b) 2)))
    (format "#%02X%02X%02X" red grn blu))
  )

(defun wave-left-xpm
  (color1 color2)
  "Return an XPM wave left."
    (create-image
     (format "/* XPM */
static char * wave_left[] = {
\"12 18 3 1\",
\"@ c %s\",
\"# c %s\",
\"  c %s\",
\"@#          \",
\"@@@         \",
\"@@@@        \",
\"@@@@#       \",
\"@@@@@       \",
\"@@@@@#      \",
\"@@@@@@      \",
\"@@@@@@      \",
\"@@@@@@#     \",
\"@@@@@@@     \",
\"@@@@@@@     \",
\"@@@@@@@#    \",
\"@@@@@@@@    \",
\"@@@@@@@@    \",
\"@@@@@@@@#   \",
\"@@@@@@@@@   \",
\"@@@@@@@@@#  \",
\"@@@@@@@@@@@#\"};"
             (if color1 color1 "None")
             (if (and color1 color2) (interpolate color1 color2) "None")
             (if color2 color2 "None"))     
     'xpm t :ascent 'center))

(defun wave-right-xpm
  (color1 color2)
  "Return an XPM wave right."  
  (create-image
   (format "/* XPM */
static char * wave_right[] = {
\"12 18 3 1\",
\"@ c %s\",
\"# c %s\",
\"  c %s\",
\"          #@\",
\"         @@@\",
\"        @@@@\",
\"       #@@@@\",
\"       @@@@@\",
\"      #@@@@@\",
\"      @@@@@@\",
\"      @@@@@@\",
\"     #@@@@@@\",
\"     @@@@@@@\",
\"     @@@@@@@\",
\"    #@@@@@@@\",
\"    @@@@@@@@\",
\"    @@@@@@@@\",
\"   #@@@@@@@@\",
\"   @@@@@@@@@\",
\"  @@@@@@@@@@\",
\"#@@@@@@@@@@@\"};"
           (if color2 color2 "None")
           (if (and color2 color1) (interpolate color2 color1) "None")
           (if color1 color1 "None"))
   'xpm t :ascent 'center))

(defun brace-left-xpm
  (color1 color2)
  "Return an XPM brace left."
  (create-image
   (format "/* XPM */
static char * brace_left[] = {
\"12 19 3 1\",
\"@ c %s\",
\"# c %s\",
\"  c %s\",
\"@#          \",
\"@@@         \",
\"@@@         \",
\"@@@         \",
\"@@@         \",
\"@@@         \",
\"@@@#        \",
\"@@@#        \",
\"@@@@#       \",
\"@@@@@@      \",
\"@@@@#       \",
\"@@@#        \",
\"@@@#        \",
\"@@@         \",
\"@@@         \",
\"@@@         \",
\"@@@         \",
\"@@@         \",
\"@#          \"};"
           (if color1 color1 "None")
           (if (and color2 color1) (interpolate color2 color1) "None")
           (if color2 color2 "None"))
   'xpm t :ascent 'center))

(defun brace-right-xpm
  (color1 color2)
  "Return an XPM brace right."
  (create-image
   (format "/* XPM */
static char * brace_right[] = {
\"12 19 3 1\",
\"@ c %s\",
\"# c %s\",
\"  c %s\",
\"          #@\",
\"         @@@\",
\"         @@@\",
\"         @@@\",
\"         @@@\",
\"         @@@\",
\"        #@@@\",
\"        #@@@\",
\"       #@@@@\",
\"      @@@@@@\",
\"       #@@@@\",
\"        #@@@\",
\"        #@@@\",
\"         @@@\",
\"         @@@\",
\"         @@@\",
\"         @@@\",
\"         @@@\",
\"          #@\"};"
           (if color2 color2 "None")
           (if (and color2 color1) (interpolate color2 color1) "None")
           (if color1 color1 "None"))
   'xpm t :ascent 'center))



(defun roundstub-left-xpm
  (color1 color2)
  "Return an XPM roundstub left."
  (create-image
   (format "/* XPM */
static char * roundstub_left[] = {
\"12 18 3 1\",
\"@ c %s\",
\"# c %s\",
\"  c %s\",
\"@@          \",
\"@@@@        \",
\"@@@@#       \",
\"@@@@@       \",
\"@@@@@       \",
\"@@@@@       \",
\"@@@@@       \",
\"@@@@@       \",
\"@@@@@       \",
\"@@@@@       \",
\"@@@@@       \",
\"@@@@@       \",
\"@@@@@       \",
\"@@@@@       \",
\"@@@@@       \",
\"@@@@#       \",
\"@@@@        \",
\"@@          \"};"
           (if color1 color1 "None")
           (if (and color2 color1) (interpolate color2 color1) "None")
           (if color2 color2 "None"))
   'xpm t :ascent 'center))

(defun roundstub-right-xpm
  (color1 color2)
  "Return an XPM roundstub right."
  (create-image
   (format "/* XPM */
static char * roundstub_right[] = {
\"12 18 3 1\",
\"@ c %s\",
\"# c %s\",
\"  c %s\",
\"          @@\",
\"        @@@@\",
\"       #@@@@\",
\"       @@@@@\",
\"       @@@@@\",
\"       @@@@@\",
\"       @@@@@\",
\"       @@@@@\",
\"       @@@@@\",
\"       @@@@@\",
\"       @@@@@\",
\"       @@@@@\",
\"       @@@@@\",
\"       @@@@@\",
\"       @@@@@\",
\"       #@@@@\",
\"        @@@@\",
\"          @@\"};"
           (if color2 color2 "None")
           (if (and color2 color1) (interpolate color2 color1) "None")
           (if color1 color1 "None"))
   'xpm t :ascent 'center))

(defun zigzag-left-xpm
  (color1 color2)
  "Return an XPM zigzag left."
  (create-image
   (format "/* XPM */
static char * zigzag_left[] = {
\"12 18 3 1\",
\"@ c %s\",
\"# c %s\",
\"  c %s\",
\"@@@@        \",
\"@@@@@       \",
\"@@@@@@      \",
\"@@@@@@@     \",
\"@@@@@@      \",
\"@@@@@       \",
\"@@@@        \",
\"@@@@@       \",
\"@@@@@@      \",
\"@@@@@@@     \",
\"@@@@@@      \",
\"@@@@@       \",
\"@@@@        \",
\"@@@@@       \",
\"@@@@@@      \",
\"@@@@@@@     \",
\"@@@@@@      \",
\"@@@@@       \"};"
           (if color1 color1 "None")
           (if (and color2 color1) (interpolate color2 color1) "None")
           (if color2 color2 "None"))
   'xpm t :ascent 'center))

(defun zigzag-right-xpm
  (color1 color2)
  "Return an XPM zigzag right."
  (create-image
   (format "/* XPM */
static char * zigzag_right[] = {
\"12 18 3 1\",
\"@ c %s\",
\"# c %s\",
\"  c %s\",
\"        @@@@\",
\"       @@@@@\",
\"      @@@@@@\",
\"     @@@@@@@\",
\"      @@@@@@\",
\"       @@@@@\",
\"        @@@@\",
\"       @@@@@\",
\"      @@@@@@\",
\"     @@@@@@@\",
\"      @@@@@@\",
\"       @@@@@\",
\"        @@@@\",
\"       @@@@@\",
\"      @@@@@@\",
\"     @@@@@@@\",
\"      @@@@@@\",
\"       @@@@@\"};"
           (if color2 color2 "None")
           (if (and color2 color1) (interpolate color2 color1) "None")
           (if color1 color1 "None"))
   'xpm t :ascent 'center))

(defun butt-left-xpm
  (color1 color2)
  "Return an XPM butt left."
  (create-image
   (format "/* XPM */
static char * butt_left[] = {
\"12 18 3 1\",
\"@ c %s\",
\"# c %s\",
\"  c %s\",
\"@@@@        \",
\"@@@@@       \",
\"@@@@@@      \",
\"@@@@@@@     \",
\"@@@@@@@     \",
\"@@@@@@@     \",
\"@@@@@@@     \",
\"@@@@@@@     \",
\"@@@@@@@     \",
\"@@@@@@@     \",
\"@@@@@@@     \",
\"@@@@@@@     \",
\"@@@@@@@     \",
\"@@@@@@@     \",
\"@@@@@@@     \",
\"@@@@@@      \",
\"@@@@@       \",
\"@@@@        \"};"
           (if color1 color1 "None")
           (if (and color2 color1) (interpolate color2 color1) "None")
           (if color2 color2 "None"))
   'xpm t :ascent 'center))

(defun butt-right-xpm
  (color1 color2)
  "Return an XPM butt right."
  (create-image
   (format "/* XPM */
static char * butt_right[] = {
\"12 18 3 1\",
\"@ c %s\",
\"# c %s\",
\"  c %s\",
\"        @@@@\",
\"       @@@@@\",
\"      @@@@@@\",
\"     @@@@@@@\",
\"     @@@@@@@\",
\"     @@@@@@@\",
\"     @@@@@@@\",
\"     @@@@@@@\",
\"     @@@@@@@\",
\"     @@@@@@@\",
\"     @@@@@@@\",
\"     @@@@@@@\",
\"     @@@@@@@\",
\"     @@@@@@@\",
\"     @@@@@@@\",
\"      @@@@@@\",
\"       @@@@@\",
\"        @@@@\"};"
           (if color2 color2 "None")
           (if (and color2 color1) (interpolate color2 color1) "None")
           (if color1 color1 "None"))
   'xpm t :ascent 'center))

(defun chamfer-xpm
  (color1 color2)
  "Return an XPM chamfer string representing."
  (create-image
   (format "/* XPM */
static char * chamfer[] = {
\"12 18 3 1\",
\"@ c %s\",
\"# c %s\",
\"  c %s\",
\"@@@@@@      \",
\"@@@@@@@     \",
\"@@@@@@@@    \",
\"@@@@@@@@@   \",
\"@@@@@@@@@   \",
\"@@@@@@@@@   \",
\"@@@@@@@@@   \",
\"@@@@@@@@@   \",
\"@@@@@@@@@   \",
\"@@@@@@@@@   \",
\"@@@@@@@@@   \",
\"@@@@@@@@@   \",
\"@@@@@@@@@   \",
\"@@@@@@@@@   \",
\"@@@@@@@@@   \",
\"@@@@@@@@@   \",
\"@@@@@@@@@   \",
\"@@@@@@@@@   \",
\"@@@@@@@@@   \"};"
           (if color1 color1 "None")
           (if (and color2 color1) (interpolate color2 color1) "None")
           (if color2 color2 "None"))
   'xpm t :ascent 'center))

(defun chamfer14-xpm
  (color1 color2)
  "Return an XPM chamfer string representing."
  (create-image
   (format "/* XPM */
static char * chamfer[] = {
\"12 14 3 1\",
\"@ c %s\",
\"# c %s\",
\"  c %s\",
\"@@@@@@      \",
\"@@@@@@@     \",
\"@@@@@@@@    \",
\"@@@@@@@@@   \",
\"@@@@@@@@@   \",
\"@@@@@@@@@   \",
\"@@@@@@@@@   \",
\"@@@@@@@@@   \",
\"@@@@@@@@@   \",
\"@@@@@@@@@   \",
\"@@@@@@@@@   \",
\"@@@@@@@@@   \",
\"@@@@@@@@@   \",
\"@@@@@@@@@   \"};"
           (if color1 color1 "None")
           (if (and color2 color1) (interpolate color2 color1) "None")
           (if color2 color2 "None"))
   'xpm t :ascent 'center))

(defun rounded-xpm
  (color1 color2)
  "Return an XPM rounded string representing."
  (create-image
   (format "/* XPM */
static char * rounded[] = {
\"12 18 3 1\",
\"@ c %s\",
\"# c %s\",
\"  c %s\",
\"@@@#        \",
\"@@@@@#      \",
\"@@@@@@@     \",
\"@@@@@@@#    \",
\"@@@@@@@@    \",
\"@@@@@@@@#   \",
\"@@@@@@@@@   \",
\"@@@@@@@@@   \",
\"@@@@@@@@@   \",
\"@@@@@@@@@   \",
\"@@@@@@@@@   \",
\"@@@@@@@@@   \",
\"@@@@@@@@@   \",
\"@@@@@@@@@   \",
\"@@@@@@@@@   \",
\"@@@@@@@@@   \",
\"@@@@@@@@@   \",
\"@@@@@@@@@   \"};"
           (if color1 color1 "None")
           (if (and color2 color1) (interpolate color2 color1) "None")
           (if color2 color2 "None"))
   'xpm t :ascent 'center))


(defun contour-left-xpm
  (color1 color2)
  "Return an XPM contour-left string representing."
  (create-image
   (format "/* XPM */
static char * contour_left[] = {
\"12 18 3 1\",
\"@ c %s\",
\"# c %s\",
\"  c %s\",
\"@           \",
\"@@#         \",
\"@@@#        \",
\"@@@@#       \",
\"@@@@@       \",
\"@@@@@#      \",
\"@@@@@@      \",
\"@@@@@@      \",
\"@@@@@@      \",
\"@@@@@@      \",
\"@@@@@@      \",
\"@@@@@@      \",
\"@@@@@@      \",
\"@@@@@@#     \",
\"@@@@@@@     \",
\"@@@@@@@#    \",
\"@@@@@@@@#   \",
\"@@@@@@@@@@@ \"};"
           (if color1 color1 "None")
           (if (and color2 color1) (interpolate color2 color1) "None")
           (if color2 color2 "None"))
   'xpm t :ascent 'center))

(defun contour-right-xpm
  (color1 color2)
  "Return an XPM contour-right string representing."
  (create-image
   (format "/* XPM */
static char * contour_right[] = {
\"12 18 3 1\",
\"@ c %s\",
\"# c %s\",
\"  c %s\",
\"           @\",
\"         #@@\",
\"        #@@@\",
\"       #@@@@\",
\"       @@@@@\",
\"      #@@@@@\",
\"      @@@@@@\",
\"      @@@@@@\",
\"      @@@@@@\",
\"      @@@@@@\",
\"      @@@@@@\",
\"      @@@@@@\",
\"      @@@@@@\",
\"     #@@@@@@\",
\"     @@@@@@@\",
\"    #@@@@@@@\",
\"   #@@@@@@@@\",
\" @@@@@@@@@@@\"};"
           (if color2 color2 "None")
           (if (and color2 color1) (interpolate color2 color1) "None")
           (if color1 color1 "None"))
   'xpm t :ascent 'center))


(defun slant-left-xpm
  (color1 color2)
  "Return an XPM left slant string representing."
  (create-image
   (format "/* XPM */
static char * slant_left[] = {
\"12 18 3 1\",
\"@ c %s\",
\"# c %s\",
\"  c %s\",
\"@@@@         \",
\"@@@@         \",
\"@@@@@        \",
\"@@@@@        \",
\"@@@@@@       \",
\"@@@@@@       \",
\"@@@@@@@      \",
\"@@@@@@@      \",
\"@@@@@@@@     \",
\"@@@@@@@@     \",
\"@@@@@@@@@    \",
\"@@@@@@@@@    \",
\"@@@@@@@@@@   \",
\"@@@@@@@@@@   \",
\"@@@@@@@@@@@  \",
\"@@@@@@@@@@@  \",
\"@@@@@@@@@@@@ \",
\"@@@@@@@@@@@@\"};"
           (if color1 color1 "None")
           (if (and color2 color1) (interpolate color2 color1) "None")
           (if color2 color2 "None"))
   'xpm t :ascent 'center))

(defun slant-right-xpm
  (color1 color2)
  "Return an XPM right slant string representing@"
  (create-image
   (format "/* XPM */
static char * slant_right[] = {
\"12 18 3 1\",
\"@ c %s\",
\"# c %s\",
\"  c %s\",
\"        @@@@\",
\"        @@@@\",
\"       @@@@@\",
\"       @@@@@\",
\"      @@@@@@\",
\"      @@@@@@\",
\"     @@@@@@@\",
\"     @@@@@@@\",
\"    @@@@@@@@\",
\"    @@@@@@@@\",
\"   @@@@@@@@@\",
\"   @@@@@@@@@\",
\"  @@@@@@@@@@\",
\"  @@@@@@@@@@\",
\" @@@@@@@@@@@\",
\" @@@@@@@@@@@\",
\"@@@@@@@@@@@@\",
\"@@@@@@@@@@@@\"};"
           (if color2 color2 "None")
           (if (and color2 color1) (interpolate color2 color1) "None")
           (if color1 color1 "None"))
   'xpm t :ascent 'center))

(defun arrow-left-xpm
  (color1 color2)
  "Return an XPM left arrow string representing@"
  (create-image
   (format "/* XPM */
static char * arrow_left[] = {
\"12 18 3 1\",
\"@ c %s\",
\"# c %s\",
\"  c %s\",
\"@           \",
\"@@          \",
\"@@@         \",
\"@@@@        \",
\"@@@@@       \",
\"@@@@@@      \",
\"@@@@@@@     \",
\"@@@@@@@@    \",
\"@@@@@@@@@   \",
\"@@@@@@@@@   \",
\"@@@@@@@@    \",
\"@@@@@@@     \",
\"@@@@@@      \",
\"@@@@@       \",
\"@@@@        \",
\"@@@         \",
\"@@          \",
\"@           \"};"
           (if color1 color1 "None")
           (if (and color2 color1) (interpolate color2 color1) "None")
           (if color2 color2 "None"))
   'xpm t :ascent 'center))

(defun arrow-right-xpm
  (color1 color2)
  "Return an XPM right arrow string representing@"
  (create-image
   (format "/* XPM */
static char * arrow_right[] = {
\"12 18 3 1\",
\"@ c %s\",
\"# c %s\",
\"  c %s\",
\"           @\",
\"          @@\",
\"         @@@\",
\"        @@@@\",
\"       @@@@@\",
\"      @@@@@@\",
\"     @@@@@@@\",
\"    @@@@@@@@\",
\"   @@@@@@@@@\",
\"   @@@@@@@@@\",
\"    @@@@@@@@\",
\"     @@@@@@@\",
\"      @@@@@@\",
\"       @@@@@\",
\"        @@@@\",
\"         @@@\",
\"          @@\",
\"           @\"};"
           (if color2 color2 "None")
           (if (and color2 color1) (interpolate color2 color1) "None")
           (if color1 color1 "None"))
   'xpm t :ascent 'center))

(defun arrow14-left-xpm
  (color1 color2)
  "Return an XPM left arrow string representing@"
  (create-image
   (format "/* XPM */
static char * arrow_left[] = {
\"12 14 3 1\",
\"@ c %s\",
\"# c %s\",
\"  c %s\",
\"@           \",
\"@@          \",
\"@@@         \",
\"@@@@        \",
\"@@@@@       \",
\"@@@@@@      \",
\"@@@@@@@     \",
\"@@@@@@@     \",
\"@@@@@@      \",
\"@@@@@       \",
\"@@@@        \",
\"@@@         \",
\"@@          \",
\"@           \"};"
           (if color1 color1 "None")
           (if (and color2 color1) (interpolate color2 color1) "None")
           (if color2 color2 "None"))
   'xpm t :ascent 'center))

(defun arrow14-right-xpm
  (color1 color2)
  "Return an XPM right arrow string representing@"
  (create-image
   (format "/* XPM */
static char * arrow_right[] = {
\"12 14 3 1\",
\"@ c %s\",
\"# c %s\",
\"  c %s\",
\"           @\",
\"          @@\",
\"         @@@\",
\"        @@@@\",
\"       @@@@@\",
\"      @@@@@@\",
\"     @@@@@@@\",
\"     @@@@@@@\",
\"      @@@@@@\",
\"       @@@@@\",
\"        @@@@\",
\"         @@@\",
\"          @@\",
\"           @\"};"
           (if color2 color2 "None")
           (if (and color2 color1) (interpolate color2 color1) "None")
           (if color1 color1 "None"))
   'xpm t :ascent 'center))

(defun curve-right-xpm
  (color1 color2)
  "Return an XPM right curve string representing@"
  (create-image
   (format "/* XPM */
static char * curve_right[] = {
\"12 18 3 1\",
\"@ c %s\",
\"# c %s\",
\"  c %s\",
\"           @\",
\"         #@@\",
\"        @@@@\",
\"       #@@@@\",
\"       @@@@@\",
\"      #@@@@@\",
\"      @@@@@@\",
\"      @@@@@@\",
\"      @@@@@@\",
\"      @@@@@@\",
\"      @@@@@@\",
\"      @@@@@@\",
\"      #@@@@@\",
\"       @@@@@\",
\"       #@@@@\",
\"        @@@@\",
\"         #@@\",
\"           @\"};"
           (if color2 color2 "None")
           (if (and color2 color1) (interpolate color2 color1) "None")
           (if color1 color1 "None"))
   'xpm t :ascent 'center))

(defun curve-left-xpm
  (color1 color2)
  "Return an XPM left curve string representing@"
  (create-image
   (format "/* XPM */
static char * curve_left[] = {
\"12 18 3 1\",
\"@ c %s\",
\"# c %s\",
\"  c %s\",
\"@           \",
\"@@#         \",
\"@@@@        \",
\"@@@@#       \",
\"@@@@@       \",
\"@@@@@#      \",
\"@@@@@@      \",
\"@@@@@@      \",
\"@@@@@@      \",
\"@@@@@@      \",
\"@@@@@@      \",
\"@@@@@@      \",
\"@@@@@#      \",
\"@@@@@       \",
\"@@@@#       \",
\"@@@@        \",
\"@@#         \",
\"@           \"};"
           (if color1 color1 "None")
           (if (and color2 color1) (interpolate color2 color1) "None")
           (if color2 color2 "None"))
   'xpm t :ascent 'center))

(defun make-xpm
  (name color1 color2 data)
  "Return an XPM image for data"
  (let 
      ((xpm
   (concat
    (format
     "/* XPM */
static char * %s[] = {
\"%i %i 2 1\",
\"@ c %s\",
\"  c %s\",
"
            (downcase (replace-regexp-in-string " " "_" name))
            (length (car data))
            (length data)
            (if color1 color1 "None")
            (if color2 color2 "None"))
    (let ((len  (length data))
          (idx  0))
      (apply 'concat
             (mapcar #'(lambda (dl)
                        (setq idx (+ idx 1))
                        (concat
                         "\""
                         (concat
                          (mapcar #'(lambda (d)
                                     (if (eq d 0)
                                         (string-to-char " ")
                                       (string-to-char "@")))
                                  dl))
                         (if (eq idx len)
                             "\"};"
                           "\",\n")))
                     data))))))
    (create-image xpm 'xpm t :ascent 'center)))

(defun percent-xpm
  (point_max point_min window_end window_start width color1 color2)
  "Draw a percent indicator based on the current        
   window (viewable) position within the buffill_endr"
  (let* ((fill_start (if (eq point_min window_start)
                 0
               (round (* 13 (/ (float window_start) (float point_max))))))
         (fill_end (if (eq point_max window_end)
                 13
               (round (* 13 (/ (float window_end) (float point_max))))))
         (o nil)
         (i 0))
    (while (< i 14)
      (setq o (cons
               (if (and (<= fill_start i)
                        (<= i fill_end))
                   (append (list 0) (make-list width 1) (list 0))
                 (append (list 0) (make-list width 0) (list 0)))
               o))
      (setq i (+ i 1)))
    (make-xpm "percent" color1 color2 (reverse o))))

;; from memoize.el @ http://nullprogram.com/blog/2010/07/26/
(defun memoize (func)
  "Memoize the given function. If argument is a symbol then
install the memoized function over the original function."
  (cl-typecase func
    (symbol (fset func (memoize-wrap (symbol-function func))) func)
    (function (memoize-wrap func))))

(defun memoize-wrap (func)
  "Return the memoized version of the given function."
  (let ((table-sym (cl-gensym))
        (val-sym (cl-gensym))
        (args-sym (cl-gensym)))
    (set table-sym (make-hash-table :test 'equal))
    `(lambda (&rest ,args-sym)
       ,(concat (documentation func) "\n(memoized function)")
       (let ((,val-sym (gethash ,args-sym ,table-sym)))
         (if ,val-sym
             ,val-sym
           (puthash ,args-sym (apply ,func ,args-sym) ,table-sym))))))

(memoize 'roundstub-left-xpm)
(memoize 'roundstub-right-xpm)
(memoize 'brace-left-xpm)
(memoize 'brace-right-xpm)
(memoize 'contour-left-xpm)
(memoize 'contour-right-xpm)
(memoize 'wave-left-xpm)
(memoize 'zigzag-left-xpm)
(memoize 'butt-left-xpm)
(memoize 'wave-right-xpm)
(memoize 'zigzag-right-xpm)
(memoize 'butt-right-xpm)
(memoize 'rounded-xpm)
(memoize 'chamfer-xpm)
(memoize 'chamfer14-xpm)
(memoize 'slant-left-xpm)
(memoize 'slant-right-xpm)
(memoize 'arrow-left-xpm)
(memoize 'arrow-right-xpm)
(memoize 'arrow14-left-xpm)
(memoize 'arrow14-right-xpm)
(memoize 'curve-left-xpm)
(memoize 'curve-right-xpm)
(memoize 'percent-xpm)

(defun main-line-make-face
  (bg &optional fg)
  (if bg
      (let ((cface (intern (concat "main-line-"
                                   bg
                                   "-"
                                   (if fg
                                       (format "%s" fg)
                                     "white")))))
        (make-face cface)2
        (if fg
            (if (eq fg 0)
                (set-face-attribute cface nil
                                    :background bg
                                    :box nil)
              (set-face-attribute cface nil
                                  :foreground fg
                                  :background bg
                                  :box nil))
          (set-face-attribute cface nil
                              :foreground "white"
                              :background bg
                              :box nil))
        cface)
    nil))

(defun main-line-make-left
  (string color1 &optional color2 localmap)
  (let ((plface (main-line-make-face color1))
        (arrow  (and color2 (not (string= color1 color2)))))
    (concat
     (if (or (not string) (string= string ""))
         ""
       (propertize " " 'face plface))
     (if string
         (if localmap
             (propertize string 'face plface 'mouse-face plface 'local-map localmap)
           (propertize string 'face plface))
       "")
     (if arrow
         (propertize " " 'face plface)
       "")
     (if arrow
         (propertize " " 'display
                     (cond
                      ((eq main-line-separator-style 'arrow           ) (arrow-left-xpm      color1 color2))
                      ((eq main-line-separator-style 'slant           ) (slant-left-xpm      color1 color2))
                      ((eq main-line-separator-style 'contour         ) (contour-left-xpm    color1 color2))
                      ((eq main-line-separator-style 'contour-left    ) (contour-left-xpm    color1 color2))
                      ((eq main-line-separator-style 'contour-right   ) (contour-right-xpm   color1 color2))
                      ((eq main-line-separator-style 'roundstub       ) (roundstub-left-xpm  color1 color2))
                      ((eq main-line-separator-style 'roundstub-left  ) (roundstub-left-xpm  color1 color2))
                      ((eq main-line-separator-style 'roundstub-right ) (roundstub-right-xpm color1 color2))
                      ((eq main-line-separator-style 'brace           ) (brace-left-xpm      color1 color2))                      
                      ((eq main-line-separator-style 'wave            ) (wave-left-xpm       color1 color2))
                      ((eq main-line-separator-style 'zigzag          ) (zigzag-left-xpm     color1 color2))
                      ((eq main-line-separator-style 'butt            ) (butt-left-xpm       color1 color2))
                      ((eq main-line-separator-style 'chamfer         ) (chamfer-xpm         color1 color2))
                      ((eq main-line-separator-style 'chamfer14       ) (chamfer14-xpm       color1 color2))
                      ((eq main-line-separator-style 'rounded         ) (rounded-xpm         color1 color2))
                      ((eq main-line-separator-style 'slant-left      ) (slant-left-xpm      color1 color2))
                      ((eq main-line-separator-style 'slant-right     ) (slant-right-xpm     color1 color2))
                      ((eq main-line-separator-style 'wave-left       ) (wave-left-xpm       color1 color2))
                      ((eq main-line-separator-style 'zigzag-left     ) (zigzag-left-xpm     color1 color2))
                      ((eq main-line-separator-style 'butt-left       ) (butt-left-xpm       color1 color2))
                      ((eq main-line-separator-style 'wave-right      ) (wave-right-xpm      color1 color2))
                      ((eq main-line-separator-style 'zigzag-right    ) (zigzag-right-xpm    color1 color2))
                      ((eq main-line-separator-style 'butt-right      ) (butt-right-xpm      color1 color2))
                      ((eq main-line-separator-style 'arrow14         ) (arrow14-left-xpm    color1 color2))
                      ((eq main-line-separator-style 'curve           ) (curve-left-xpm      color1 color2))
                      (t
                       (arrow-left-xpm color1 color2)))
                     'local-map (make-mode-line-mouse-map
                                 'mouse-1 (lambda () (interactive)
                                            (setq main-line-separator-style (get-next-separator-style))
                                            (force-mode-line-update))))
       ""))))

(defun main-line-make-right
  (string color2 &optional color1 localmap)
  (let ((plface (main-line-make-face color2))
        (arrow  (and color1 (not (string= color1 color2)))))
    (concat
     (if arrow
         (propertize " " 'display
                     (cond
                      ((eq main-line-separator-style 'arrow           ) (arrow-right-xpm     color1 color2))
                      ((eq main-line-separator-style 'slant           ) (slant-right-xpm     color1 color2))
                      ((eq main-line-separator-style 'contour         ) (contour-right-xpm   color1 color2))
                      ((eq main-line-separator-style 'contour-left    ) (contour-left-xpm    color1 color2))
                      ((eq main-line-separator-style 'contour-right   ) (contour-right-xpm   color1 color2))
                      ((eq main-line-separator-style 'roundstub       ) (roundstub-right-xpm color1 color2))
                      ((eq main-line-separator-style 'roundstub-left  ) (roundstub-left-xpm  color1 color2))
                      ((eq main-line-separator-style 'roundstub-right ) (roundstub-right-xpm color1 color2))
                      ((eq main-line-separator-style 'brace           ) (brace-right-xpm     color1 color2))
                      ((eq main-line-separator-style 'wave            ) (wave-right-xpm      color1 color2))
                      ((eq main-line-separator-style 'zigzag          ) (zigzag-right-xpm    color1 color2))
                      ((eq main-line-separator-style 'butt            ) (butt-right-xpm      color1 color2))
                      ((eq main-line-separator-style 'rounded         ) (rounded-xpm         color1 color2))
                      ((eq main-line-separator-style 'chamfer         ) (chamfer-xpm         color1 color2))
                      ((eq main-line-separator-style 'chamfer14       ) (chamfer14-xpm       color1 color2))
                      ((eq main-line-separator-style 'slant-left      ) (slant-left-xpm      color1 color2))
                      ((eq main-line-separator-style 'slant-right     ) (slant-right-xpm     color1 color2))
                      ((eq main-line-separator-style 'wave-left       ) (wave-left-xpm       color1 color2))
                      ((eq main-line-separator-style 'zigzag-left     ) (zigzag-left-xpm     color1 color2))
                      ((eq main-line-separator-style 'butt-left       ) (butt-left-xpm       color1 color2))
                      ((eq main-line-separator-style 'wave-right      ) (wave-right-xpm      color1 color2))
                      ((eq main-line-separator-style 'zigzag-right    ) (zigzag-right-xpm    color1 color2))
                      ((eq main-line-separator-style 'butt-right      ) (butt-right-xpm      color1 color2))
                      ((eq main-line-separator-style 'arrow14         ) (arrow14-right-xpm   color1 color2))
                      ((eq main-line-separator-style 'curve           ) (curve-right-xpm     color1 color2))
                      (t
                       (arrow-right-xpm color1 color2)))
                     'local-map (make-mode-line-mouse-map
                                 'mouse-1 (lambda () (interactive)
                                            (setq main-line-separator-style (get-next-separator-style))
                                            (force-mode-line-update))))
       "")
     (if arrow
         (propertize " " 'face plface)
       "")
     (if string
         (if localmap
             (propertize string 'face plface 'mouse-face plface 'local-map localmap)
           (propertize string 'face plface))
       "")
     (if (or (not string) (string= string ""))
         ""
       (propertize " " 'face plface)))))

(defun get-next-separator-style ()
  (cond ((eq main-line-separator-style 'chamfer)         'chamfer14)
        ((eq main-line-separator-style 'chamfer14)       'brace)
        ((eq main-line-separator-style 'brace)           'rounded)
        ((eq main-line-separator-style 'rounded)         'zigzag)
        ((eq main-line-separator-style 'zigzag)          'wave)
        ((eq main-line-separator-style 'wave)            'butt)
        ((eq main-line-separator-style 'butt)            'arrow)
        ((eq main-line-separator-style 'arrow)           'contour)
        ((eq main-line-separator-style 'contour)         'contour-left)
        ((eq main-line-separator-style 'contour-left)    'contour-right)
        ((eq main-line-separator-style 'contour-right)   'roundstub)
        ((eq main-line-separator-style 'roundstub)       'roundstub-left)
        ((eq main-line-separator-style 'roundstub-left)  'roundstub-right)
        ((eq main-line-separator-style 'roundstub-right) 'slant)
        ((eq main-line-separator-style 'slant)           'slant-left)
        ((eq main-line-separator-style 'slant-left)      'slant-right)
        ((eq main-line-separator-style 'slant-right)     'wave-left)
        ((eq main-line-separator-style 'wave-left)       'zigzag-left)
        ((eq main-line-separator-style 'zigzag-left)     'butt-left)
        ((eq main-line-separator-style 'butt-left)       'wave-right)
        ((eq main-line-separator-style 'wave-right)      'zigzag-right)
        ((eq main-line-separator-style 'zigzag-right)    'butt-right)
        ((eq main-line-separator-style 'butt-right)      'arrow14)
        ((eq main-line-separator-style 'arrow14)         'curve)
        ((eq main-line-separator-style 'curve)           'chamfer)
        (t                                               'chamfer))
  )

(defun main-line-make-fill
  (color)
  ;; justify right by filling with spaces to right fringe, 20 should be calculated
  (let ((plface (main-line-make-face color)))
    (if (eq 'right (get-scroll-bar-mode))
        (propertize " " 'display '((space :align-to (- right-fringe 21)))
                    'face plface)
      (propertize " " 'display '((space :align-to (- right-fringe 24)))
                  'face plface))))

(defun main-line-make-text
  (string color &optional fg localmap)
  (let ((plface (main-line-make-face color)))
    (if string
        (if localmap
            (propertize string 'face plface 'mouse-face plface 'local-map localmap)
          (propertize string 'face plface))
      "")))

(defun main-line-make (side string color1 &optional color2 localmap)
  (cond ((and (eq side 'right) color2) (main-line-make-right  string color1 color2 localmap))
        ((and (eq side 'left) color2)  (main-line-make-left   string color1 color2 localmap))
        ((eq side 'left)               (main-line-make-left   string color1 color1 localmap))
        ((eq side 'right)              (main-line-make-right  string color1 color1 localmap))
        (t                             (main-line-make-text   string color1 localmap))))

(defmacro defmain-line (name string)
  `(defun ,(intern (concat "main-line-" (symbol-name name)))
     (side color1 &optional color2)
     (main-line-make side
                    ,string
                    color1 color2)))

(defun main-line-mouse (click-group click-type string)
  (cond ((eq click-group 'minor)
         (cond ((eq click-type 'menu)
                `(lambda (event)
                   (interactive "@e")
                   (minor-mode-menu-from-indicator ,string)))
               ((eq click-type 'help)
                `(lambda (event)
                   (interactive "@e")
                   (describe-minor-mode-from-indicator ,string)))
               (t
                `(lambda (event)
                   (interactive "@e")
                   nil))))
        (t
         `(lambda (event)
            (interactive "@e")
            nil))))

(defmain-line arrow       "")

(defmain-line buffer-id   (propertize (car (propertized-buffer-identification "%12b"))
                                     'face (main-line-make-face color1)))

(defmain-line buffer-size (propertize
                          (if main-line-buffer-size-suffix
                              "%I"
                            "%i")
                          'local-map (make-mode-line-mouse-map
                                      'mouse-1 (lambda () (interactive)
                                                 (setq main-line-buffer-size-suffix
                                                       (not main-line-buffer-size-suffix))
                                                 (force-mode-line-update)))))
(defmain-line rmw "%*")

(defmain-line major-mode
  (propertize mode-name
              'help-echo "Major mode\n\ mouse-1: Display major mode menu\n\ mouse-2: Show help for major mode\n\ mouse-3: Toggle minor modes"
              'local-map (let ((map (make-sparse-keymap)))
                           (define-key map [mode-line down-mouse-1]
                             `(menu-item ,(purecopy "Menu Bar") ignore
                                         :filter (lambda (_) (mouse-menu-major-mode-map))))
                           (define-key map [mode-line mouse-2] 'describe-mode)
                           (define-key map [mode-line down-mouse-3] mode-line-mode-menu)
                           map)))

(defmain-line minor-modes
  (let ((mms (split-string (format-mode-line minor-mode-alist))))
    (apply 'concat
           (mapcar #'(lambda (mm)
                      (propertize (if (string= (car mms) mm)
                                      mm
                                    (concat " " mm))
                                  'help-echo "Minor mode\n mouse-1: Display minor mode menu\n mouse-2: Show help for minor mode\n mouse-3: Toggle minor modes"
                                  'local-map (let ((map (make-sparse-keymap)))
                                               (define-key map [mode-line down-mouse-1]   (main-line-mouse 'minor 'menu mm))
                                               (define-key map [mode-line mouse-2]        (main-line-mouse 'minor 'help mm))
                                               (define-key map [mode-line down-mouse-3]   (main-line-mouse 'minor 'menu mm))
                                               (define-key map [header-line down-mouse-3] (main-line-mouse 'minor 'menu mm))
                                               map))) mms))))
(defmain-line row "%4l")
(defmain-line column "%3c")
(defmain-line percent "%6p")

(defmain-line narrow (let (real-point-min real-point-max)
                      (save-excursion
                        (save-restriction
                          (widen)
                          (setq real-point-min (point-min) real-point-max (point-max))))
                      (when (or (/= real-point-min (point-min))
                                (/= real-point-max (point-max)))
                        (propertize "Narrow"
                                    'help-echo "mouse-1: Remove narrowing from the current buffer"
                                    'local-map (make-mode-line-mouse-map
                                                'mouse-1 'mode-line-widen)))))
(defmain-line status      "%s")
(defmain-line emacsclient mode-line-client)
(defmain-line vc vc-mode)

(defmain-line
  percent-xpm
  (propertize "  "
              'display
              (let (pmax
                    pmin
                    (ws (window-start))
                    (we (window-end)))
                (save-restriction
                  (widen)
                  (setq pmax (point-max))
                  (setq pmin (point-min)))
                (percent-xpm
                 pmax pmin
                 we ws 3
                 color1 color2))))

(setq-default
 mode-line-format
 (list "%e"
       '(:eval (concat
                (main-line-rmw            'left   nil  )
                (main-line-buffer-id      'left   nil  main-line-color1  )
                (main-line-major-mode     'left        main-line-color1  )
                (main-line-minor-modes    'left        main-line-color1  )
                (main-line-narrow         'left        main-line-color1   main-line-color2  )
                (main-line-vc             'center                        main-line-color2  )
                (main-line-make-fill                                     main-line-color2  )
                (main-line-row            'right       main-line-color1   main-line-color2  )
                (main-line-make-text      ":"          main-line-color1  )
                (main-line-column         'right       main-line-color1  )
                (main-line-percent-xpm    'right  nil  main-line-color1  )               
                (main-line-make-text      "  "    nil  )))))

(provide 'main-line)

;;; main-line.el ends here
