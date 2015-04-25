 Use the commands defined here to examine or save the background or
 foreground color at the text cursor or the mouse pointer.

 After using commands `eyedrop-pick-background-*' or
 `eyedrop-pick-foreground-*', the picked color is saved in variable
 `eyedrop-picked-background' or `eyedrop-picked-foreground',
 respectively.

 If you have Emacs 22 or later, all of the functionality provided
 here, and much more, is provided in library `palette.el'.  Use
 library `eyedropper' instead of `palette.el' if either of these
 applies:

 * You do not want to use the color palette itself.  You want only
   the functionality provided by `eyedropper.el'.

 * Your Emacs version is older than Emacs 22 (`palette.el' requires
   22 or later).

 If you load `palette.el', there is no reason to also load
 `eyedropper.el'.  However, if for some reason you do load both
 `palette.el' and `eyedropper.el' then load `palette.el' second, so
 that its definitions will override those provided in
 `eyedropper.el', providing additional functionality for the color
 palette.

 To use this library:

   Add this to your initialization file (~/.emacs or ~/_emacs):

     (require 'eyedropper) ; Load this library.

   You will also need my library `hexrgb.el'; it is loaded
   automatically by `eyedropper.el'.  Get it here:
   http://www.emacswiki.org/cgi-bin/wiki/hexrgb.el.

 Commands defined here:

   `background-color', `eyedrop-background-at-mouse',
   `eyedrop-background-at-point', `eyedrop-foreground-at-mouse',
   `eyedrop-foreground-at-point', `eyedropper-background',
   `eyedropper-foreground', `eyedrop-pick-background-at-mouse',
   `eyedrop-pick-background-at-point',
   `eyedrop-pick-foreground-at-mouse',
   `eyedrop-pick-foreground-at-point', `foreground-color',
   `pick-background-color', `pick-foreground-color'.

 Non-interactive functions defined here:

   `eyedrop-color-message', `eyedrop-face-at-point', `keywordp'.

 Internal variables defined here:

   `eyedrop-last-picked-color', `eyedrop-picked-background',
   `eyedrop-picked-foreground'.
