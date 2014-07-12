On OSX, if you use Cocoa Emacs' daemon mode and then close all GUI
frames, the Emacs app on your dock becomes nonfunctional until you
open a new GUI frame using emacsclient on the command line. This is
obviously suboptimal. This package makes it so that whenever you
close the last GUI frame, a new frame is created and the Emacs app
is hidden, thus approximating the behvaior of daemon mode while
keeping the Emacs dock icon functional. To actually quit instead of
hiding Emacs, use CMD+Q (or Alt+Q if you swapped Alt & Command
keys).
