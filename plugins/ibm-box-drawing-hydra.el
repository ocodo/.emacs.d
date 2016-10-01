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
;; I built this for me, and while it's GPL, I keep it in my config,
;; and don't have any particular intent to spread it.  If you found
;; it, enjoy.
;;
;; Also good is aa2u mode - which converts ascii boxes to
;; unicode (or as I think of them `IBM Box chars' - from Code Page 437)
;;
;; Sample boxes:
;;
;; ┌─────┬───┬────────┐
;; │     │   ├─────┐  │
;; │     ╞═══╪═════╧══╡
;; └─────┴───┴────────┘
;;
;; ╔═══════╤═══╗  ┌───┐╔═══╕
;; ╠═══════╪═══╬══╪═══╪╬═══╡
;; ║       │   ║  │   │║   │
;; ╚═══════╧═══╝  ╘═══╧╩═══╛
;;
;;; Licence: GPL v3

;;; Code:
(defcustom ibm-box--overwrite nil "Overwrite mode for IBM (codepage 437) box drawing.")

(defun ibm-box--insert (char)
  "Insert CHAR with conditional overwrite."
  (interactive)
  (when ibm-box--overwrite
    (kill-char 1))
  (insert char))

(bind-key "C-x r B"
 (defhydra ibm-draw-box-hydra (:color pink :hint nil) "
IBM Box Chars  _r_ ─         _R_ ═         _v_ │         _V_ ║
(CodePage 437) _q_ ┌ _w_ ┬ _e_ ┐ _Q_ ╒ _W_ ╤ _E_ ╕ _t_ ╔ _y_ ╦ _u_ ╗ _T_ ╓ _Y_ ╥ _U_ ╖  _C-q_ ╭ ╮ _C-w_
               _a_ ├ _s_ ┼ _d_ ┤ _A_ ╞ _S_ ╪ _D_ ╡ _g_ ╠ _h_ ╬ _j_ ╣ _G_ ╟ _H_ ╫ _J_ ╢
               _z_ └ _x_ ┴ _c_ ┘ _Z_ ╘ _X_ ╧ _C_ ╛ _b_ ╚ _n_ ╩ _m_ ╝ _B_ ╙ _N_ ╨ _M_ ╜  _C-a_ ╰ ╯ _C-s_
_ESC_ to exit    _i_ Toggle Overwrite/Insert
"
   ("ESC" nil nil :color blue) ;; Esc to exit.
   ("<space>"   (search-backward "+"))
   ("S-<space>" (search-forward "+"))
   ("q" (ibm-box--insert "┌")) ("w" (ibm-box--insert "┬")) ("e" (ibm-box--insert "┐"))
   ("Q" (ibm-box--insert "╒")) ("W" (ibm-box--insert "╤")) ("E" (ibm-box--insert "╕"))
   ("t" (ibm-box--insert "╔")) ("y" (ibm-box--insert "╦")) ("u" (ibm-box--insert "╗"))
   ("T" (ibm-box--insert "╓")) ("Y" (ibm-box--insert "╥")) ("U" (ibm-box--insert "╖"))
   ("a" (ibm-box--insert "├")) ("s" (ibm-box--insert "┼")) ("d" (ibm-box--insert "┤"))
   ("A" (ibm-box--insert "╞")) ("S" (ibm-box--insert "╪")) ("D" (ibm-box--insert "╡"))
   ("g" (ibm-box--insert "╠")) ("h" (ibm-box--insert "╬")) ("j" (ibm-box--insert "╣"))
   ("G" (ibm-box--insert "╟")) ("H" (ibm-box--insert "╫")) ("J" (ibm-box--insert "╢"))
   ("z" (ibm-box--insert "└")) ("x" (ibm-box--insert "┴")) ("c" (ibm-box--insert "┘"))
   ("Z" (ibm-box--insert "╘")) ("X" (ibm-box--insert "╧")) ("C" (ibm-box--insert "╛"))
   ("b" (ibm-box--insert "╚")) ("n" (ibm-box--insert "╩")) ("m" (ibm-box--insert "╝"))
   ("B" (ibm-box--insert "╙")) ("N" (ibm-box--insert "╨")) ("M" (ibm-box--insert "╜"))
   ("r" (ibm-box--insert "─")) ("R" (ibm-box--insert "═"))
   ("v" (ibm-box--insert "│")) ("V" (ibm-box--insert "║"))
   ("C-q" (ibm-box--insert "╭")) ("C-w" (ibm-box--insert "╮"))
   ("C-a" (ibm-box--insert "╰")) ("C-s" (ibm-box--insert "╯"))
   ("i" (setq ibm-box--overwrite (not ibm-box--overwrite)) )))

(provide 'ibm-box-drawing-hydra)
;;; ibm-box-drawing-hydra.el ends here
