This package defines functions that turn string into ansi colored
strings.

You can paint strings (see `ansi-colors' for all possible text
colors).

  (ansi-red "foo")
  (ansi-black "bar")


You can paint a string backgrounds (see `ansi-on-colors' for all
possible background colors).

  (ansi-on-blue "foo")
  (ansi-on-green "bar")


You can add styles to a string (see `ansi-styles' for all possible
styles).

  (ansi-bold "foo")
  (ansi-blink "bar")


You can use `with-ansi', which allows for a simplified DSL.

  (with-ansi
   (red "foo")
   (black "bar"))

  (with-ansi
   (on-blue "foo")
   (on-green "bar"))

  (with-ansi
   (bold "foo")
   (blink "bar"))


If you want to add multiple effects on a single string, you can use
nesting:

  (ansi-bold
   (ansi-red "foo"))

  (with-ansi
   (bold
    (red "foo")))


Before adding effects on strings, the ansi functions first passes
their arguments to the `format' function. This means you can write
like this:

  (ansi-bold "%d passed, %d failed" passed failed)
