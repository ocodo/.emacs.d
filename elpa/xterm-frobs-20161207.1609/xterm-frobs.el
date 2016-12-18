;;; xterm-frobs.el --- manipulate xterm when running emacs in tty mode
;; Package-Version: 20161207.1609

;; Copyright (C) 1998, 99, 00, 2004 Noah S. Friedman

;; Author: Noah Friedman <friedman@splode.com>
;; Maintainer: friedman@splode.com
;; Created: 1998-03-21

;; $Id: xterm-frobs.el,v 1.9 2016/11/24 20:24:53 friedman Exp $

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Commands for tty-mode emacs when running under xterm, to manipulate the
;; parent xterm state.  Some of these commands may only work with post-1996
;; XFree86 xterm versions.

;; Color defaults and algorithms derived from the xterm-189 (2004-5-16)
;; distribution from http://dickey.his.com/xterm/
;; In particular see XTerm-col.ad and 256colres.pl
;; These values may differ from those of earlier releases of xterm.

;; Updates of xterm-frobs.el may be retrieved via
;; http://www.splode.com/~friedman/software/emacs-lisp/

;;; Code:

;; If emacs is running under the `screen' tty session management program
;; (which does its own terminal emulation), the sequence sent to the
;; terminal must be wrapped in a Device Control String so that the original
;; control sequence will be passed uninterpreted to the parent xterm.
(defvar xterm-screen-dcs-encapsulation
  (not (null (or (getenv "STY")
                 (save-match-data
                   (string-match "^screen\\(\\|-.*\\)$" (getenv "TERM")))))))

(defvar xterm-standard-16color-alist
  '(("black"       0     0     0     0)
    ("red3"        1 52685     0     0)
    ("green3"      2     0 52685     0)
    ("yellow3"     3 52685 52685     0)
    ("dodgerblue1" 4  7710 37008 65535)
    ("magenta3"    5 52685     0 52685)
    ("cyan3"       6     0 52685 52685)
    ("gray90"      7 58853 58853 58853)
    ("gray50"      8 32639 32639 32639)
    ("red"         9 65535     0     0)
    ("green"      10     0 65535     0)
    ("yellow"     11 65535 65535     0)
    ("steelblue1" 12 25443 47288 65535)
    ("magenta"    13 65535     0 65535)
    ("cyan"       14     0 65535 65535)
    ("white"      15 65535 65535 65535))
  "The first 16 standard xterm colors and their corresponding RGB values.")


;;;###autoload
(defun xterm-iconify ()
  "Minimize \(iconify\) xterm window."
  (interactive)
  (xterm-send-escape-sequence "\e[2t"))

;;;###autoload
(defun xterm-deiconify ()
  "Restore \(deiconify\) xterm window."
  (interactive)
  (xterm-send-escape-sequence "\e[1t"))


;;;###autoload
(defun xterm-set-font (font-name)
    "Set the font of the xterm window to FONT.
When called interactively, prompt for the name of the font to use.

This function is used to change the font of the xterm window in which a
tty-mode emacs is running.  It should also work if emacs is running under
`screen' in an xterm window.

Use \\[set-default-font] if this emacs is using the window system directly."
  (interactive "sFont name: ")
  (xterm-send-escape-sequence (format "\e]50;%s\a" font-name)))

;;;###autoload
(defun xterm-set-icon-title (title)
  "Set the title in the icon for this xterm window to TITLE.
This does not change the title of the corresponding window."
  (interactive "sIcon title: ")
  (xterm-send-escape-sequence (format "\e]1;%s\a" title)))

;;;###autoload
(defun xterm-set-window-title (title)
  "Set the title for xterm window to TITLE.
This does not change the title in the corresponding icon."
  (interactive "sWindow title: ")
  (xterm-send-escape-sequence (format "\e]2;%s\a" title)))

;;;###autoload
(defun xterm-set-all-titles (title)
  "Set the title for xterm window and corresponding icon to TITLE."
  (interactive "sIcon and window title: ")
  (xterm-send-escape-sequence (format "\e]0;%s\a" title)))


;;;###autoload
(defun xterm-set-background-color (color)
  (interactive "sBackground color: ")
  (xterm-send-escape-sequence (format "\e]10;%s\a" color)))

;;;###autoload
(defun xterm-set-foreground-color (color)
  (interactive "sForeground color: ")
  (xterm-send-escape-sequence (format "\e]11;%s\a" color)))

;;;###autoload
(defun xterm-set-cursor-color (color)
  (interactive "sCursor color: ")
  (xterm-send-escape-sequence (format "\e]12;%s\a" color)))

;;;###autoload
(defun xterm-set-mouse-foreground-color (color)
  (interactive "sMouse foreground color: ")
  (xterm-send-escape-sequence (format "\e]13;%s\a" color)))

;;;###autoload
(defun xterm-set-mouse-background-color (color)
  (interactive "sMouse background color: ")
  (xterm-send-escape-sequence (format "\e]14;%s\a" color)))

(defun xterm-set-Tek-foreground-color (color)
  (interactive "sTek foreground color: ")
  (xterm-send-escape-sequence (format "\e]15;%s\a" color)))

(defun xterm-set-Tek-background-color (color)
  (interactive "sTek background color: ")
  (xterm-send-escape-sequence (format "\e]16;%s\a" color)))

;;;###autoload
(defun xterm-set-highlight-color (color)
  (interactive "sHighlight color: ")
  (xterm-send-escape-sequence (format "\e]17;%s\a" color)))

;;;###autoload
(defun xterm-reverse-video ()
  "Set xterm to reverse video mode.
For monochrome xterms, this is white foreground on black background.
For xterms which support color, this has the effect of swapping the
foreground and background colors, whatever they may be.

The effect of this command and \\[xterm-normal-video] may be exchanged
if the XTerm*reverseVideo resource property is set to True."
  (interactive)
  (xterm-send-escape-sequence "\e[?5h"))

;;;###autoload
(defun xterm-normal-video ()
  "Set xterm to normal \(i.e. non-reverse\) video mode.
For monochrome xterms, this is black foreground on white background.
For xterms which support color, this has the effect of restoring the
original foreground and background colors, whatever they may be.

The effect of this command and \\[xterm-reverse-video] may be exchanged
if the XTerm*reverseVideo resource property is set to True."
  (interactive)
  (xterm-send-escape-sequence "\e[?5l"))

(defun xterm-aaa-mode (&optional prefix)
  "Color-emulate an Ann Arbor Ambassador.
With negative prefix argument, reset foreground to white."
  (interactive "p")
  (cond ((> prefix 0)
         (xterm-set-background-color "black")
         (xterm-set-foreground-color "green")
         (xterm-set-cursor-color "green")
         (xterm-set-mouse-foreground-color "black")
         (xterm-set-mouse-background-color "green"))
        (t
         (xterm-set-background-color "black")
         (xterm-set-foreground-color "white")
         (xterm-set-cursor-color "white")
         (xterm-set-mouse-foreground-color "black")
         (xterm-set-mouse-background-color "white"))))


(defun xterm-report-cursor-position ()
  "Return the position of the xterm cursor.
The result is a cons of the form \(COL . ROW\) indicating the offset in
characters from the upper left-hand corner of the window."
  (interactive)
  (and (interactive-p)
       (sit-for 0))
  (let* ((re "\e\\[\\([0-9]+\\);\\([0-9]+\\)R")
         (result (xterm-send-and-read-response "\e[6n" re t))
         (pos (cons (string-to-number (xterm-substring 2 result))
                    (string-to-number (xterm-substring 1 result)))))
    (and (interactive-p)
         (message "COL=%d, ROW=%d" (car pos) (cdr pos)))
    pos))

(defun xterm-report-window-state ()
  "Return 'iconified or 'non-iconified."
  (let ((result (xterm-send-and-read-response "\e[11t" "\e\\[[0-9]+t")))
    (cond ((string= result "\e[1t")
           'non-iconified)
          ((string= result "\e[2t")
           'iconified))))

(defun xterm-report-window-position-pixels ()
  "Return the position of the xterm window on the display.
The result is a cons of the form \(X . Y\) indicating the offset in
pixels from the upper left-hand corner of the display.
Origin is (1 . 1)."
  (interactive)
  (let* ((re "\e\\[3;\\([0-9]+\\);\\([0-9]+\\)t")
         (result (xterm-send-and-read-response "\e[13t" re t))
         (pos (cons (string-to-number (xterm-substring 1 result))
                    (string-to-number (xterm-substring 2 result)))))
    (and (interactive-p)
         (message "X=%d, Y=%d" (car pos) (cdr pos)))
    pos))

(defun xterm-report-window-size-pixels ()
  "Return the size of the xterm window, in pixels.
The result is a cons \(WIDTH . HEIGHT\) indicating the dimensions."
  (interactive)
  (let* ((re "\e\\[4;\\([0-9]+\\);\\([0-9]+\\)t")
         (result (xterm-send-and-read-response "\e[14t" re t))
         (dim (cons (string-to-number (xterm-substring 2 result))
                    (string-to-number (xterm-substring 1 result)))))
    (and (interactive-p)
         (message "%dx%d" (car dim) (cdr dim)))
    dim))

(defun xterm-report-window-size ()
  "Return the size of the xterm window, in character cells.
The result is a cons \(WIDTH . HEIGHT\) indicating the dimensions."
  (interactive)
  (let* ((re "\e\\[8;\\([0-9]+\\);\\([0-9]+\\)t")
         (result (xterm-send-and-read-response "\e[18t" re t))
         (dim (cons (string-to-number (xterm-substring 2 result))
                    (string-to-number (xterm-substring 1 result)))))
    (and (interactive-p)
         (message "%dx%d" (car dim) (cdr dim)))
    dim))

(defun xterm-report-icon-title ()
  "Return the icon label string associated with the xterm window."
  (interactive)
  (let* ((re "\e\\]L\\(.*\\)\e\\\\")
         (result (xterm-send-and-read-response "\e[20t" re t))
         (title (xterm-substring 1 result)))
    (and (interactive-p)
         (message "%s" title))
    title))

(defun xterm-report-window-title ()
  "Return the window title string associated with the xterm window."
  (interactive)
  (let* ((re "\e\\]l\\(.*\\)\e\\\\")
         (result (xterm-send-and-read-response "\e[21t" re t))
         (title (xterm-substring 1 result)))
    (and (interactive-p)
         (message "%s" title))
    title))


(defun xterm-report-color-cells ()
  "Return the number of colors which can be displayed by xterm window."
  (interactive)
  ;; Termcap query (`+q') capability is a compile-time option; if not
  ;; enabled, xterm will not respond at all.  Therefore, also query for
  ;; DECSTBM (terminal scrolling region) so as to guarantee a response.
  ;; If it turns out that the terminal does not implement this capability,
  ;; just return emacs' current notion of the number of colors.
  (let* ((reply (xterm-send-and-read-response
                 "\eP+q436f\e\\\eP$qr\e\\" ; "436f" is hex ascii for "Co"
                 "\eP.\$r[0-9;]*r\e\\\\"))
         (result (and (string-match "\eP1\\+r436f=\\([0-9a-f]+\\)\e\\\\" reply)
                      (xterm-substring 1 reply)))
         num-str (i 0) j)
    (cond (result
           (setq num-str (make-string (/ (length result) 2) ?0))
           (while (< i (length num-str))
             (setq j (* i 2))
             (aset num-str i
                   (string-to-number (substring result j (+ 2 j)) 16))
             (setq i (1+ i)))
           (setq result (string-to-number num-str)))
          (t
           (setq result (display-color-cells))))
    (and (interactive-p)
         (message "%d" result))
    result))

;; queries terminal for rgb data and returns unparsed blocks
(defun xterm-color-rgb-data (&optional query-blocksize max-colors verbose)
  (let* ((max (or max-colors (xterm-report-color-cells)))
         (start 0)
         (stop nil)
         (step (1- (min max (or query-blocksize 32))))
         (data nil))
    (while (< start max)
      (setq stop (+ start step))
      (and verbose
           (message "Looking up terminal color indices %d-%d ..." start stop))
      (setq data (cons
                  (xterm-send-and-read-response
                   (mapconcat 'identity
                              (list "\e]4"
                                    (mapconcat (lambda (n) (format "%d;?" n))
                                               (number-sequence start stop)
                                               ";")
                                    "\e\\")
                              ";")
                   (format "\e\\]4;%d;rgb:[0-9a-f/]+\e\\\\" stop))
                  data))
      (setq start (1+ stop)))
    (nreverse data)))

;; This is derived from tty-color-approximate, but it works from
;; color-name-rgb-alist, not what emacs currently thinks are the defined
;; colors.  (we can't just let-bind variables because the format of the rgb
;; alist is incompatible)
(defun xterm-color-approximate (rgb)
  (let* ((color-list (when (boundp 'color-name-rgb-alist)
                       color-name-rgb-alist))
	 (candidate (car color-list))
	 (best-distance 195076)	;; 3 * 255^2 + 15
	 (r (ash (car rgb) -8))
	 (g (ash (cadr rgb) -8))
	 (b (ash (nth 2 rgb) -8))
	 best-color)
    (while candidate
      (let ((try-rgb (cdr candidate))
	    ;; If the approximated color is not close enough to the
	    ;; gray diagonal of the RGB cube, favor non-gray colors.
	    ;; (The number 0.065 is an empirical ad-hoc'ery.)
	    (favor-non-gray (>= (tty-color-off-gray-diag r g b) 0.065))
	    try-r try-g try-b
	    dif-r dif-g dif-b dist)
	;; If the RGB values of the candidate color are unknown, we
	;; never consider it for approximating another color.
	(if try-rgb
	    (progn
	      (setq try-r (lsh (car try-rgb) -8)
		    try-g (lsh (cadr try-rgb) -8)
		    try-b (lsh (nth 2 try-rgb) -8))
	      (setq dif-r (- r try-r)
		    dif-g (- g try-g)
		    dif-b (- b try-b))
	      (setq dist (+ (* dif-r dif-r) (* dif-g dif-g) (* dif-b dif-b)))
	      (if (and (< dist best-distance)
		       ;; The candidate color is on the gray diagonal
		       ;; if its RGB components are all equal.
		       (or (/= try-r try-g) (/= try-g try-b)
			   (not favor-non-gray)))
		  (setq best-distance dist
			best-color candidate)))))
      (setq color-list (cdr color-list))
      (setq candidate (car color-list)))
    best-color))

;; Fill matching substrings into a statically allocated buffer to avoid
;; excessive consing, since we're just converting immediately to numbers.
(defsubst xterm-color-substring-to-number (string matchnum base buffer)
  (let* ((beg (match-beginning matchnum))
         (mpos (1- (match-end matchnum)))
         (bpos (1- (length buffer))))
    (while (>= mpos beg)
      (aset buffer bpos (aref string mpos))
      (setq bpos (1- bpos)
            mpos (1- mpos)))
    (while (>= bpos 0)
      (aset buffer bpos ?0)
      (setq bpos (1- bpos))))
  (string-to-number buffer base))

(defun xterm-color-map (&optional max-colors)
  "Query xterm for current color palette and return corresponding alist.
This function depends on xterm supporting certain escape sequences which
not all versions implement."
  (let* ((rgb-data (xterm-color-rgb-data nil max-colors))
         (map nil)
         (color-name nil)
         (cvt-buffer (make-string 4 ?0))
         data start-pos idx rgb
         (case-fold-search t)
         ;; Reduce frequency of garbage collection until we're finished.
         (gc-cons-threshold (max gc-cons-threshold (* 5 1024 1024))))
    (while rgb-data
      (setq data      (car rgb-data)
            rgb-data  (cdr rgb-data)
            start-pos 0)
      (while (string-match "\e\\]4;\\([0-9]+\\);rgb:\\([0-9a-f]+\\)/\\([0-9a-f]+\\)/\\([0-9a-f]+\\)\e\\\\"
                           data start-pos)
        (setq idx (xterm-color-substring-to-number data 1 nil cvt-buffer)
              rgb (list (xterm-color-substring-to-number data 2 16 cvt-buffer)
                        (xterm-color-substring-to-number data 3 16 cvt-buffer)
                        (xterm-color-substring-to-number data 4 16 cvt-buffer))
              start-pos (match-end 0))

        (setq color-name
              (or (when (boundp 'color-name-rgb-alist)
                    (car (rassoc rgb color-name-rgb-alist)))
                  (car (xterm-color-approximate rgb))
                  (format "color-%d" idx)))
        (setq map (cons (cons color-name (cons idx rgb)) map))))
    (nreverse map)))

;;;###autoload
(defun xterm-sync-emacs-colors ()
  "Query xterm for color palette and define color list for Emacs."
  (interactive)
  (setq tty-defined-color-alist (xterm-color-map))
  (clear-face-cache))

;;;###autoload
(defun xterm-print-formatted-color-alist (&optional color-alist)
  "Create a pretty-printed table of the current xterm color map.
This table is inserted into a new buffer which can be saved to a file and
reloaded later."
  (interactive)
  (let* ((map (mapcar (lambda (elt) (mapcar 'prin1-to-string elt))
                      (or color-alist tty-defined-color-alist)))
         (lengths (make-vector (length (car map)) 0))
         fmt
         (buf (get-buffer-create "*Defined Colors*")))

    (mapc (lambda (list)
            (let ((i 0))
              (mapc (lambda (elt)
                      (aset lengths i (max (aref lengths i) (length elt)))
                      (setq i (1+ i)))
                    list)))
          map)
    (aset lengths 0 (- (aref lengths 0)))

    (setq fmt (format "(%s)\n"
                      (mapconcat (lambda (d) (format "%%%ds" d)) lengths " ")))

    (save-excursion
      (set-buffer buf)
      (setq buffer-read-only nil)
      (erase-buffer)
      (fundamental-mode)
      (insert (format-time-string ";; Generated %Y-%m-%d %H:%M:%S %Z"))
      (when (interactive-p)
        (insert (format " by `%s'" this-command))
        (let ((file (symbol-file this-command)))
          (when file
            (insert "\n;; in " (file-name-sans-extension
                            (file-name-nondirectory file)) ".el"))))
      (insert "\n\n(setq tty-defined-color-alist\n'")
      (princ (mapcar (lambda (elt) (apply 'format fmt elt)) map) buf)
      (search-backward "\n")
      (delete-char 1)
      (goto-char (point-max))
      (insert ")\n\n(clear-face-cache)\n\n;; eof\n")
      (emacs-lisp-mode)
      (xterm-lisp-indent-region (point-min) (point-max)))
    (when (interactive-p)
      (pop-to-buffer buf)
      (message "Save this buffer to a file, then load it"))))

;; Emacs post 23.1 removes lisp-indent-region
(defun xterm-lisp-indent-region (start end)
  "Indent every line whose first char is between START and END inclusive."
  (save-excursion
    (let ((endmark (copy-marker end)))
      (goto-char start)
      (and (bolp) (not (eolp))
           (lisp-indent-line))
      (indent-sexp endmark)
      (set-marker endmark nil))))

(defun xterm-standard-256color-alist ()
  "Generate the default 256-color map used by xterm.
Return an alist suitable to use as the value for `tty-defined-color-alist',
which see.

This map is the list of RGB values compiled into xterm itself; it may not
correspond to the actual color map used by xterm if it has been overridden
in X resources or other mechanisms."
  (let ((alist nil)
        (ir 0))
    ;; Colors 16-231 are a 6x6x6 color cube
    (while (< ir 6)
      (let ((ig 0))
        (while (< ig 6)
          (let ((ib 0))
            (while (< ib 6)
              (let ((i (+ 16 (* ir 36) (* ig 6) ib))
                    (r (if (zerop ir) 0 (+ 55 (* ir 40))))
                    (g (if (zerop ig) 0 (+ 55 (* ig 40))))
                    (b (if (zerop ib) 0 (+ 55 (* ib 40)))))
                (setq alist (cons (list (format "color-%d" i) i
                                        (logior (lsh r 8) r) ; cvt to 16bit
                                        (logior (lsh g 8) g)
                                        (logior (lsh b 8) b))
                                  alist)))
              (setq ib (1+ ib))))
          (setq ig (1+ ig))))
      (setq ir (1+ ir)))

    ;; Colors 232-255 are a grayscale ramp, leaving out black and white
    (let ((i 232)
          (grey 0)
          (level 0))
      (while (< i 256)
        (setq grey (- i 232))
        (setq level (+ (* grey 10) 8))
        (setq level (logior (lsh level 8) level)) ; cvt to 16bit
        (setq alist (cons (list (format "color-%d" i) i
                                level level level)
                          alist))
        (setq i (1+ i))))

    (nconc (mapcar 'copy-sequence xterm-standard-16color-alist)
           (nreverse alist))))


;; This implementation currently reads answerback data into a preallocated
;; string to avoid excessive consing.  Using an actual buffer would
;; probably be overkill given the typical length of answerbacks.
;; The major exception is when reading xterm's supported color list.
(defun xterm-send-and-read-response (string response-re &optional alter-md)
  (xterm-send-escape-sequence string)
  (let ((response (make-string 32 0))
        (strlen 32) ; initial buffer size
        (c 0)       ; index into buffer
        (match-data (match-data)))
    (while (not (string-match response-re response))
      (cond ((= c strlen)
             (setq response (concat response (make-string strlen 0)))
             (setq strlen (* 2 strlen))))
      (aset response c (read-char))
      (setq c (1+ c)))
    (or alter-md
        (store-match-data match-data))
    (substring response 0 c)))

(defun xterm-send-escape-sequence (string)
  (cond ((and xterm-screen-dcs-encapsulation
              (save-match-data (string-match "\e[P\\\\]" string)))
         ;; ^[P and ^[\ are screen's DCS text string encapsulators, so if
         ;; they appear as a literal in the string argument, they would
         ;; truncate/corrupt the encapsulation.  Therefore, any literal
         ;; occurences will be broken up into multiple encapsulations so as
         ;; not to confuse screen.
         (save-match-data
           (let ((pos 0)
                 (substrings nil))
             (while (string-match "\e\\(P\\|\\\\\\)" string pos)
               (setq substrings
                     (cons "\e\\"
                           (cons (substring string pos (match-beginning 1))
                                 (cons "\eP" substrings))))
               (setq pos (match-beginning 1)))
             (setq substrings (cons (substring string pos) substrings))
             (setq string (mapconcat 'identity (nreverse substrings) "")))))
        (xterm-screen-dcs-encapsulation
         (setq string (format "\eP%s\e\\" string))))
  (send-string-to-terminal string))

(defun xterm-substring (n &optional string)
  "Return substring matched by last search.
N specifies which match data pair to use
Value is nil if there is no Nth match.
If STRING is not specified, the current buffer is used."
  (if (match-beginning n)
      (if string
	  (substring string (match-beginning n) (match-end n))
	(buffer-substring (match-beginning n) (match-end n)))))

(provide 'xterm-frobs)

;;; xterm-frobs.el ends here
