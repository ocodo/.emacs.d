# Super quick start guide...

Assuming you have `git` installed...

In a terminal window, type:

    git clone --recursive  git://github.com/ocodo/emacs.d.git ~/.emacs.d

(You need `--recursive` to grab all the submodules I'm using.)

Now start Emacs.app - if you run in a Terminal, I suggest iTerm2 and
set the Terminal mode to `xterm-256color`

# Ocodo emacs dot, *'proper'* intro

Ocodo's dot emacs directory might make Emacs a little less painful
than the defaults.

This set up is primarily used on a Mac, with Emacs 24. (Cocoa emacs or
Emacs Mac Port work just fine)

# Get it...

I'll assume you don't already have a `.emacs` or `.emacs.d` in your
home folder, if you do, just stop, look at what I'm doing in `init.el`
and rip out what you want to use, or **backup your dot-emacs** and
have a play, chances are I'm doing something that will irritate you,
no one's config will be better than yours, but there might be
something cool you want to use too.

Open your Terminal... (Unix / Mac)

    cd ~
    git clone --recursive --depth 1 git://github.com/ocodo/emacs.d.git .emacs.d

Run Emacs and away you go...

### Here's a few places that should help you on your voyage of discovery...

* http://www.masteringemacs.org/
* http://emacsrocks.com/
* http://www.emacswiki.org/

And of course, the official emacs tour and manual...

* Emacs Tour - http://www.gnu.org/software/emacs/tour/ - this is a must read...
* Emacs Manual - http://www.gnu.org/software/emacs/manual/html_mono/emacs.html
* The Emacs manual as a PDF: http://www.gnu.org/software/emacs/manual/emacs.pdf
