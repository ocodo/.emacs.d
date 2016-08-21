;;; ibm-box-drawing-hydra --- draw DOS style boxes with a hydra

;;; Commentary:
;; Use M-x load-library ibm-box-drawing-hydra (after placing this file in the Emacs lisp load-path)

;; Draw box with IBM single line box characters. - activate with C-x r S-B
;;
;; Press Esc to exit.
;;
;; Essentially treats the left hand on a QWERTY layout
;; as the box (i.e. QWE,ASD,ZXC)
;; Horizontal and vertical lines are on R -> horz and V -> vert
;; All other keys are available so you can move and
;; space as required.  The bindings are active until you hit Esc.
;;
;; You can use Artist mode for things like this too, this is just yet
;; another way.
;;
;; I built this for me, and while it's GPL, I keep it in my config.
;;
;; Also good is aa2u mode - which converts artist-mode boxes to
;; unicode (or to me `IBM Box chars' - aka. Code Page 437)

;;; Licence: GPL v3

;;; Code:
(defcustom ibm-box--overwrite nil "Overwrite mode for IBM (codepage 437) box drawing.")

(defun ibm-box--insert (char)
  "Insert CHAR with conditional overwrite."
  (interactive)
  (when ibm-box--overwrite
    (kill-char 1))
  (insert char))

(global-set-key (kbd "C-x r B")
                  (defhydra ibm-draw-box-hydra (:color pink) "Draw box using IBM Code Page 437"
                    ("ESC" nil "exit\n" :color blue) ;; Esc to exit.
                    ("<space>"   (search-backward "+")  "jump to prev \"+\"")
                    ("S-<space>" (search-forward "+")  "next \"+\"\n")
                    ("q" (ibm-box--insert "┌") "┌") ("w" (ibm-box--insert "┬") "┬") ("e" (ibm-box--insert "┐") "┐ - ")
                    ("Q" (ibm-box--insert "╒") "╒") ("W" (ibm-box--insert "╤") "╤") ("E" (ibm-box--insert "╕") "╕ - ")
                    ("t" (ibm-box--insert "╔") "╔") ("y" (ibm-box--insert "╦") "╦") ("u" (ibm-box--insert "╗") "╗ - ")
                    ("T" (ibm-box--insert "╓") "╓") ("Y" (ibm-box--insert "╥") "╥") ("U" (ibm-box--insert "╖") "╖\n")
                    ("a" (ibm-box--insert "├") "├") ("s" (ibm-box--insert "┼") "┼") ("d" (ibm-box--insert "┤") "┤ - ")
                    ("A" (ibm-box--insert "╞") "╞") ("S" (ibm-box--insert "╪") "╪") ("D" (ibm-box--insert "╡") "╡ - ")
                    ("g" (ibm-box--insert "╠") "╠") ("h" (ibm-box--insert "╬") "╬") ("j" (ibm-box--insert "╣") "╣ - ")
                    ("G" (ibm-box--insert "╟") "╟") ("H" (ibm-box--insert "╫") "╫") ("J" (ibm-box--insert "╢") "╢\n")
                    ("z" (ibm-box--insert "└") "└") ("x" (ibm-box--insert "┴") "┴") ("c" (ibm-box--insert "┘") "┘ - ")
                    ("Z" (ibm-box--insert "╘") "╘") ("X" (ibm-box--insert "╧") "╧") ("C" (ibm-box--insert "╛") "╛ - ")
                    ("b" (ibm-box--insert "╚") "╚") ("n" (ibm-box--insert "╩") "╩") ("m" (ibm-box--insert "╝") "╝ - ")
                    ("B" (ibm-box--insert "╙") "╙") ("N" (ibm-box--insert "╨") "╨") ("M" (ibm-box--insert "╜") "╜\n")
                    ("r" (ibm-box--insert "─") "─") ("R" (ibm-box--insert "│") "│                                      ")
                    ("i" (ibm-box--insert "═") "═") ("I" (ibm-box--insert "║") "║\n")
                    ("C-q" (ibm-box--insert "╭") "╭") ("C-w" (ibm-box--insert "╮") "╮\n")
                    ("C-a" (ibm-box--insert "╰") "╰") ("C-s" (ibm-box--insert "╯") "╯\n")
                    ("<f9>" (setq ibm-box--overwrite (not ibm-box--overwrite))
                     (format "Toggle overwrite mode [%s]"
                             (if  ibm-box--overwrite "ON" "OFF")))))

(provide 'ibm-box-drawing-hydra)
;;; ibm-box-drawing-hydra.el ends here
