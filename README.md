# Super quick start guide...

Assuming you have `git` installed...

In a terminal window, type:

    git clone git://github.com/ocodo/emacs.d.git ~/.emacs.d


Now start Emacs.app - if you run in a Terminal, I suggest iTerm2 and set the Terminal mode to `xterm-256color`

# Ocodo emacs dot, *'proper'* intro

You want to the power of Emacs, but you're scared silly of it?

Never fear, Ocodo's dot emacs directory will make it all a little less painful, this is a purist free zone, and frankly, if you're already configuring Emacs to be superhardcore, I suggest you buy those foot-pedals, forget about other peoples dotfiles and sharpen up your emacs lisp skills.

## Still here?

Ok, I guess you're interested enough to proceed... 

This set up is primarily used on a Mac, with Cocoa Emacs 24.1rc, get that version if you want to be fully compatible. However, I use it quite happily on a few linux/unix boxes, without many issues, however, I do customize several Mac key shortcuts, using the Cmd key (e.g. `Cmd-o` opens a new file) .. you've been warned.

Also worth noting is CUA mode is active, that's the `C-x` `C-v` `C-c` keys for Cut/Copy/Paste and `C-z` for undo, it also includes a great rectangle mode, which you can start with `C-enter`.

## M-x 

M-x is my favourite thing about Emacs, and I think for a lot of other people too. M-x let's you access every single interactive function that's active in Emacs, (there's hundreds of non-interactive ones too, but I'm going to tell you to go in search of knowledge yourself now... at least on the topic of EmacsLisp.)

# Get it...

I'll assume you don't already have a `.emacs` or `.emacs.d` in your home folder, if you do, just stop, look at what I'm doing in `init.el` and rip out what you want to use, or **backup your dot-emacs** and have a play, chances are I'm doing something that will irritate you, no one's config will be better than yours, but there might be something cool you want to use too. 

Open your Terminal... (Unix / Mac) 

    cd ~
    git clone git://github.com/ocodo/emacs.d.git .emacs.d

Run Emacs and away you go...

# Contributing?

Well, this isn't something I'll want to pull changes on personally, however, feel very free to fork and do what you like.

# Anything else?

Well, I like the Icons I made and there's a color-theme editor over at http://jasonm23.github.com

Other than that, I strongly encourage you to journey deep into Emacs, it's very interesting and powerful, and thanks to really nice displays and font rendering on the Mac it can even look nice too!

### Here's a few places that should help you on your voyage of discovery...

* http://www.masteringemacs.org/
* http://emacsrocks.com/
* http://www.emacswiki.org/

And of course, the official emacs tour and manual... 

* Emacs Tour - http://www.gnu.org/software/emacs/tour/ - this is a must read...
* Emacs Manual - http://www.gnu.org/software/emacs/manual/html_mono/emacs.html 
* The Emacs manual as a PDF: http://www.gnu.org/software/emacs/manual/emacs.pdf

# Disclaimer.

These dotfiles are tested on OS X, they work on other platforms too, but don't do any specific customizations for them (e.g. cygwin etc.) but any POSIX compliant OS should be fine.

Having said that, I use them most with http://emacsformacosx.com/emacs-builds/Emacs-24.1-universal-10.6.8.dmg and http://emacsformacosx.com/emacs-builds/Emacs-23.3-universal-10.6.6.dmg on Lion and Mt.Lion Dev Preview, with good results.
