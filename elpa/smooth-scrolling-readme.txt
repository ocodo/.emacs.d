Make emacs scroll smoothly, keeping the point away from the top and
bottom of the current buffer's window in order to keep lines of
context around the point visible as much as possible, whilst
avoiding sudden scroll jumps which are visually confusing.

This is a nice alternative to all the native scroll-* custom
variables, which unfortunately cannot provide this functionality
perfectly.  `scroll-margin' comes closest, but has some bugs
(e.g. with handling of mouse clicks).  See

  http://www.emacswiki.org/cgi-bin/wiki/SmoothScrolling

for the gory details.
