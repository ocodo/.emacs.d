This is a fork of powerline.el which I began while the original
authorship of powerline was unknown,

(powerline.el 1.0.0 was posted to Emacswiki by Nicolas Rougier)

Using mainline.el.

Add a require to .emacs / init.el :

    (require 'mainline)

You can customize the separator graphic by setting the custom variable

    mainline-arrow-shape

possible values...

* chamfer
* chamfer14 (default)
* rounded
* arrow
* arrow-14
* slant
* slant-left
* slant-right
* half
* percent
* curve

For screenshots and additional info see the article at
emacsfodder.github.com/blog/powerline-enhanced/
