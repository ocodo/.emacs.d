   Starting with 24.4 (24.3.50.1 as of 20121212) this library is
   no longer required; the changes have been merged into Emacs.

This package extends `button' by adding support for adding buttons to
the header line.  Since the header line is very limited compared to a
buffer most of the functionality provided by `button' is not available
for buttons in the header line.

While `button' provides the function `insert-button' (as well as
others) to insert a button into a buffer at point, something similar
can't be done here, due to the lack of point in header lines.

Instead use `header-button-format' like this:

(setq header-line-format
      (concat "Here's a button: "
              (header-button-format "Click me!" :action 'my-action)))

Like with `button' you can create your own derived button types:

(define-button-type 'my-header
  :supertype 'header
  :action 'my-action)

(setq header-line-format
      (concat (header-button-format "Click me!" :action 'my-action) " "
              (header-button-format "No me!" :type 'my-header)))

The function associated with `:action' is called with the button plist
as only argument.  Do no use `plist-get' to extract a value from it.
Instead use `header-button-get' which will also extract values stored
in it's type.

(defun my-action (button)
  (message "This button labeled `%s' belongs to category `%s'"
           (header-button-label button)
           (header-button-get button 'category)))
