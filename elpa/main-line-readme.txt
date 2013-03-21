Note Milkbox/MELPA users this is v1.2.7

This is a fork of powerline.el which I began while the original
authorship of powerline was unknown,

-- Using main-line.el.

Add a require to .emacs / init.el

    (require 'main-line)

You can loop through the different separator styles by clicking on
them (directly on the separator)

Or customize it by setting the custom variable:

    main-line-separator-style

e.g.

    (setq main-line-separator-style 'wave)

possible values...

- contour
- contour-left
- contour-right
- roundstub
- roundstub-left
- roundstub-right
- brace
- wave
- zigzag
- butt
- wave-left
- zigzag-left
- butt-left
- wave-right
- zigzag-right
- butt-right
- chamfer
- chamfer14
- rounded
- arrow
- arrow14
- slant
- slant-left
- slant-right
- curve

To customize the modeline - simply override the value of mode-line-format,
see the default at the end of the script, as an example.

You can create your own modeline additions by using the defmain-line macro.

for example,

(defmain-line row "%4l")

gives you main-line-row to use in mode-line-format

Note. main-line-percent-xpm requires 18px separators (use
main-line-percent with arrow14 or chamfer14)
