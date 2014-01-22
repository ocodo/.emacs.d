Provides forward/back stack for point.  I use load it like so:

(add-to-list 'load-path "/home/matt/work/emacs/point-stack")
(require 'point-stack)
(global-set-key '[(f5)] 'point-stack-push)
(global-set-key '[(f6)] 'point-stack-pop)
(global-set-key '[(f7)] 'point-stack-forward-stack-pop)

Then when I know I'm going to want to come back to where I am I hit
f5.  This stores the location of of the point.  When I want to come
back to that point hit f6.  I can go forward by hitting f7.

based on http://www.emacswiki.org/emacs/JohnConnors
enhanced with forward-stack
