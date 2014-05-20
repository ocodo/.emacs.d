Commentary:

Extend M-x all to be editable M-x occur:
  - Show line number before line content (using overlay)
  - Can navigate with M-x next-error / M-x previous-error

Call M-x all from anything/helm:
  1. Call anything/helm command showing lineno and content
     such as M-x anything-occur / anything-browse-code /
             helm-occur / helm-browse-code etc
  2. Press C-c C-a to show anything/helm contents into *All* buffer
  3. You can edit *All* buffer!

Installation:

Put all-ext.el to your load-path.
The load-path is usually ~/elisp/.
It's set in your ~/.emacs like this:
(add-to-list 'load-path (expand-file-name "~/elisp"))

And the following to your ~/.emacs startup file.

(require 'all-ext)
optional
(require 'helm-config) ;; or (require 'anything-config)

No need more.
