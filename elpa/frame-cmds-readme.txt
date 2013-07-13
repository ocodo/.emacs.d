   Frame and window commands (interactive functions).


 Summary:

   Load this library from your init file (~/.emacs or _emacs).
   Add the suggested key bindings (below) to  your init file.
   Use `M-up|down|left|right' to move frames around incrementally.
   Use `C-S-v', `M-S-v', `C-S-next', `C-S-prior' to move frames to screen edges.
   Use `C-M-up|down|left|right' to resize frames incrementally.
   Use `C-M-z' or `C-x C-z' to iconify/hide all frames.
   Use `C-M-z' in a lone frame to restore all frames.
   Use `C-mouse-1' in the minibuffer to restore all frames.
   Use `C-mouse-1' in Dired to mark/unmark a file.
   Use `C-mouse-3' on the mode line to remove window from frame.
   Use `tile-frames-horizontally', `-vertically' to tile frames.
   Use `C-x o' to select `other-window' or `other-frame'.

 Commands to incrementally resize frames are `enlarge-frame' and
 `enlarge-frame-horizontally'.  Sarir Khamsi
 [sarir.khamsi@raytheon.com] originally wrote `enlarge-frame',
 which he called `sk-grow-frame'.

 Note on saving changes made with the commands defined here:

   Some of the commands defined here change frame properties.
   You can save any changes you have made, by using Customize.
   To visit a Customize buffer of all unsaved changes you have
   made, use command `customize-customized'.

   Frame parameter changes, such as background color, can be saved
   for future use by all frames or all frames of a certain
   kind.  For that, you must change the frame parameters of the
   correponding frame-alist variable.

   There is no single variable for saving changes to parameters of
   the current frame.  Instead, there are several different
   frame-alist variables, which you can use to define different
   kinds of frames.  These include: `default-frame-alist',
   `initial-frame-alist', and `special-display-frame-alist'.  The
   complete list of such frame alist variables is available using
   function `frame-alist-var-names', defined here.

   Example: Suppose you change the background color of a frame and
   want to make that the default background color for new frames in
   the future.  You will need to update the value of variable
   `default-frame-alist' to use the `background-color' parameter
   setting of the changed frame.

   You can easily copy one or all parameter values from any given
   frame to any frame alist (such as `default-frame-alist'), by
   using the commands `set-frame-alist-parameter-from-frame' and
   `set-all-frame-alist-parameters-from-frame'.  Those commands are
   defined here.

 NOTE: If you also use library `fit-frame.el', and you are on MS
 Windows, then load that library before `frame-cmds.el'.  The
 commands `maximize-frame' and `restore-frame' defined here are
 more general and non-Windows-specific than the commands of the
 same name defined in `fit-frame.el'.


 User options defined here:

   `available-screen-pixel-bounds', `enlarge-font-tries',
   `frame-config-register', `frame-parameters-to-exclude',
   `move-frame-wrap-within-display-flag'
   `rename-frame-when-iconify-flag', `show-hide-show-function',
   `window-mgr-title-bar-pixel-height'.

 Commands defined here:

   `delete-1-window-frames-on', `delete/iconify-window',
   `delete/iconify-windows-on', `delete-other-frames',
   `delete-windows-for', `enlarge-font', `enlarge-frame',
   `enlarge-frame-horizontally', `hide-everything', `hide-frame',
   `iconify-everything', `iconify/map-frame', `iconify/show-frame',
   `jump-to-frame-config-register', `maximize-frame',
   `maximize-frame-horizontally', `maximize-frame-vertically',
   `mouse-iconify/map-frame', `mouse-iconify/show-frame',
   `mouse-remove-window', `mouse-show-hide-mark-unmark',
   `move-frame-down', `move-frame-left', `move-frame-right',
   `move-frame-to-screen-bottom', `move-frame-to-screen-left',
   `move-frame-to-screen-right', `move-frame-to-screen-top',
   `move-frame-to-screen-top-left', `move-frame-up',
   `other-window-or-frame', `remove-window', `remove-windows-on',
   `rename-frame', `rename-non-minibuffer-frame', `restore-frame',
   `restore-frame-horizontally', `restore-frame-vertically',
   `save-frame-config',
   `set-all-frame-alist-parameters-from-frame',
   `set-frame-alist-parameter-from-frame', `show-*Help*-buffer',
   `show-a-frame-on', `show-buffer-menu', `show-frame',
   `show-hide', `shrink-frame', `shrink-frame-horizontally',
   `tell-customize-var-has-changed', `tile-frames',
   `tile-frames-horizontally', `tile-frames-vertically',
   `toggle-max-frame', `toggle-max-frame-horizontally',
   `toggle-max-frame-vertically'.

 Non-interactive functions defined here:

   `assq-delete-all' (Emacs 20), `available-screen-pixel-bounds',
   `available-screen-pixel-height', `available-screen-pixel-width',
   `butlast' (Emacs 20), `effective-screen-pixel-bounds',
   `enlarged-font-name', `frame-alist-var-names',
   `frame-cmds-set-difference', `frame-iconified-p',
   `frame-parameter-names', `nbutlast' (Emacs 20),
   `new-frame-position', `read-args-for-tile-frames',
   `read-buffer-for-delete-windows', `smart-tool-bar-pixel-height'.

 Error symbols defined here:

   `font-too-small', `font-size'.



 ***** NOTE: The following EMACS PRIMITIVES have been REDEFINED HERE:

 `delete-window' - If only one window in frame, `delete-frame'.
 `delete-windows-on' -
    1) Reads buffer differently.  Only buffers showing windows are candidates.
    2) Calls `delete-window', so this also deletes frames where
       window showing the BUFFER is the only window.
       (That's true also for vanilla Emacs 23+, but not before.)


 Suggested key bindings:

  (global-set-key [(meta up)]                    'move-frame-up)
  (global-set-key [(meta down)]                  'move-frame-down)
  (global-set-key [(meta left)]                  'move-frame-left)
  (global-set-key [(meta right)]                 'move-frame-right)
  (global-set-key [(meta shift ?v)]              'move-frame-to-screen-top)      ; like `M-v'
  (global-set-key [(control shift ?v)]           'move-frame-to-screen-bottom)   ; like `C-v'
  (global-set-key [(control shift prior)]        'move-frame-to-screen-left)     ; like `C-prior'
  (global-set-key [(control shift next)]         'move-frame-to-screen-right)    ; like `C-next'
  (global-set-key [(control shift home)]         'move-frame-to-screen-top-left)
  (global-set-key [(control meta down)]          'enlarge-frame)
  (global-set-key [(control meta right)]         'enlarge-frame-horizontally)
  (global-set-key [(control meta up)]            'shrink-frame)
  (global-set-key [(control meta left)]          'shrink-frame-horizontally)
  (global-set-key [(control ?x) (control ?z)]    'iconify-everything)
  (global-set-key [vertical-line S-down-mouse-1] 'iconify-everything)
  (global-set-key [(control ?z)]                 'iconify/show-frame)
  (global-set-key [mode-line mouse-3]            'mouse-iconify/show-frame)
  (global-set-key [mode-line C-mouse-3]          'mouse-remove-window)
  (global-set-key [(control meta ?z)]            'show-hide)
  (global-set-key [vertical-line C-down-mouse-1] 'show-hide)
  (global-set-key [C-down-mouse-1]               'mouse-show-hide-mark-unmark)
  (substitute-key-definition 'delete-window      'remove-window global-map)
  (define-key ctl-x-map "o"                      'other-window-or-frame)
  (define-key ctl-x-4-map "1"                    'delete-other-frames)
  (define-key ctl-x-5-map "h"                    'show-*Help*-buffer)
  (substitute-key-definition 'delete-window      'delete-windows-for global-map)
  (define-key global-map "\C-xt."                'save-frame-config)
  (define-key ctl-x-map "o"                      'other-window-or-frame)

  (defalias 'doremi-prefix (make-sparse-keymap))
  (defvar doremi-map (symbol-function 'doremi-prefix) "Keymap for Do Re Mi commands.")
  (define-key global-map "\C-xt" 'doremi-prefix)
  (define-key doremi-map "." 'save-frame-config)

 Customize the menu.  Uncomment this to try it out.

  (defvar menu-bar-frames-menu (make-sparse-keymap "Frames"))
  (define-key global-map [menu-bar frames]
    (cons "Frames" menu-bar-frames-menu)))
  (define-key menu-bar-frames-menu [set-all-params-from-frame]
    '(menu-item "Set All Frame Parameters from Frame" set-all-frame-alist-parameters-from-frame
      :help "Set frame parameters of a frame to their current values in frame"))
  (define-key menu-bar-frames-menu [set-params-from-frame]
    '(menu-item "Set Frame Parameter from Frame..." set-frame-alist-parameter-from-frame
      :help "Set parameter of a frame alist to its current value in frame"))
  (define-key menu-bar-frames-menu [separator-frame-1] '("--"))
  (define-key menu-bar-frames-menu [tile-frames-vertically]
    '(menu-item "Tile Frames Vertically..." tile-frames-vertically
      :help "Tile all visible frames vertically"))
  (define-key menu-bar-frames-menu [tile-frames-horizontally]
    '(menu-item "Tile Frames Horizontally..." tile-frames-horizontally
      :help "Tile all visible frames horizontally"))
  (define-key menu-bar-frames-menu [separator-frame-2] '("--"))
  (define-key menu-bar-frames-menu [toggle-max-frame-vertically]
    '(menu-item "Toggle Max Frame Vertically" toggle-max-frame-vertically
      :help "Maximize or restore the selected frame vertically"
      :enable (frame-parameter nil 'restore-height)))
  (define-key menu-bar-frames-menu [toggle-max-frame-horizontally]
    '(menu-item "Toggle Max Frame Horizontally" toggle-max-frame-horizontally
      :help "Maximize or restore the selected frame horizontally"
      :enable (frame-parameter nil 'restore-width)))
  (define-key menu-bar-frames-menu [toggle-max-frame]
    '(menu-item "Toggle Max Frame" toggle-max-frame
      :help "Maximize or restore the selected frame (in both directions)"
      :enable (or (frame-parameter nil 'restore-width) (frame-parameter nil 'restore-height))))
  (define-key menu-bar-frames-menu [maximize-frame-vertically]
    '(menu-item "Maximize Frame Vertically" maximize-frame-vertically
      :help "Maximize the selected frame vertically"))
  (define-key menu-bar-frames-menu [maximize-frame-horizontally]
    '(menu-item "Maximize Frame Horizontally" maximize-frame-horizontally
      :help "Maximize the selected frame horizontally"))
  (define-key menu-bar-frames-menu [maximize-frame]
    '(menu-item "Maximize Frame" maximize-frame
      :help "Maximize the selected frame (in both directions)"))
  (define-key menu-bar-frames-menu [separator-frame-3] '("--"))
  (define-key menu-bar-frames-menu [iconify-everything]
    '(menu-item "Iconify All Frames" iconify-everything
      :help "Iconify all frames of session at once"))
  (define-key menu-bar-frames-menu [show-hide]
    '(menu-item "Hide Frames / Show Buffers" show-hide
      :help "Show, if only one frame visible; else hide.")))

  (defvar menu-bar-doremi-menu (make-sparse-keymap "Do Re Mi"))
  (define-key global-map [menu-bar doremi]
    (cons "Do Re Mi" menu-bar-doremi-menu))
  (define-key menu-bar-doremi-menu [doremi-font+]
    '("Save Frame Configuration" . save-frame-config))

 See also these files for other frame commands:

    `autofit-frame.el' - Automatically fit each frame to its
                         selected window.  Uses `fit-frame.el'.

    `fit-frame.el'     - 1) Fit a frame to its selected window.
                         2) Incrementally resize a frame.

    `doremi-frm.el'    - Incrementally adjust frame properties
                         using arrow keys and/or mouse wheel.

    `thumb-frm.el'     - Shrink frames to a thumbnail size and
                         restore them again.

    `zoom-frm.el'      - Zoom a frame or buffer, so that its text
                         appears larger or smaller.
