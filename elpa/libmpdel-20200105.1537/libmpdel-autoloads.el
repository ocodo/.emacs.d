;;; libmpdel-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "libmpdel" "libmpdel.el" (0 0 0 0))
;;; Generated autoloads from libmpdel.el

(autoload 'libmpdel-connect-profile "libmpdel" "\
Connect to MPD server defined in PROFILE.
Interactively, let the user choose PROFILE from `libmpdel-profiles'.

If a connection already exists, terminate it first.

\(fn PROFILE)" t nil)

(autoload 'libmpdel-playback-set-volume "libmpdel" "\
Set volume to VOLUME.

\(fn VOLUME)" t nil)

(autoload 'libmpdel-playback-next "libmpdel" "\
Play next song in the playlist." t nil)

(autoload 'libmpdel-playback-previous "libmpdel" "\
Play previous song in the playlist." t nil)

(autoload 'libmpdel-play "libmpdel" "\
Start playing." t nil)

(autoload 'libmpdel-stop "libmpdel" "\
Stop playing.  See also `libmpdel-playback-play-pause'." t nil)

(autoload 'libmpdel-playback-play-pause "libmpdel" "\
Toggle between play and pause.
See also `libmpdel-playback-stop'." t nil)

(autoload 'libmpdel-playback-seek "libmpdel" "\
Seeks to the position TIME within the current song.

TIME is a string indicating a number of seconds, fractions
allowed.  If prefixed by + or -, then the time is relative to
the current playing position.

If HANDLER is non-nil, execute it with no parameter when seek
succeeds.

\(fn TIME &optional HANDLER)" t nil)

(autoload 'libmpdel-playback-set-random "libmpdel" "\
Set playback mode to random." t nil)

(autoload 'libmpdel-playback-unset-random "libmpdel" "\
Set playback mode to sequential (not random)." t nil)

(autoload 'libmpdel-playback-set-repeat "libmpdel" "\
Set playback mode to repeat." t nil)

(autoload 'libmpdel-playback-unset-repeat "libmpdel" "\
Set playback mode to sequential (not repeat)." t nil)

(autoload 'libmpdel-playback-set-single-forever "libmpdel" "\
Set playback single mode to forever.
This will repeat the current song forever." t nil)

(autoload 'libmpdel-playback-set-single-never "libmpdel" "\
Set playback single mode to never.
This will not repeat the current song." t nil)

(autoload 'libmpdel-playback-set-single-once "libmpdel" "\
Set playback single mode to once.
This will repeat the current song only once and then keep playing
the current playlist." t nil)

(autoload 'libmpdel-database-update "libmpdel" "\
Update the music database for URI, everything if nil.
Updates the music database: find new files, remove deleted files,
update modified files.

URI is a particular directory or song/file to update.  If you do
not specify it, everything is updated.

\(fn &optional URI)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "libmpdel" '("libmpdel-")))

;;;***

;;;### (autoloads nil "libmpdel-directory" "libmpdel-directory.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from libmpdel-directory.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "libmpdel-directory" '("libmpdel-directory--")))

;;;***

;;;### (autoloads nil nil ("libmpdel-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; libmpdel-autoloads.el ends here
