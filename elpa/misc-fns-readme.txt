   Miscellaneous non-interactive functions.

 Code here is organized into sections by area affected.

   Sections are separated by `;;;$ ... ---------------'.

 You may want to put this in your `~/.emacs' file, to erase the
 minibuffer when it is inactive and `minibuffer-empty-p':

  (require 'misc-fns)
  (add-hook '<mode>-hook 'notify-user-of-mode), for each <mode>.


 Face defined here: `notifying-user-of-mode'.

 User options (variables) defined here:

   `buffer-modifying-cmds', `mode-line-reminder-duration',
   `notifying-user-of-mode-flag'.

 Functions defined here:

   `another-buffer', `color-named-at', `current-line',
   `display-in-mode-line', `do-files', `flatten', `fontify-buffer',
   `force-time-redisplay', `interesting-buffer-p',
   `live-buffer-name', `make-transient-mark-mode-buffer-local',
   `mode-ancestors', `mod-signed', `notify-user-of-mode',
   `region-or-buffer-limits', `signum', `undefine-keys-bound-to',
   `undefine-killer-commands', `unique-name'.
