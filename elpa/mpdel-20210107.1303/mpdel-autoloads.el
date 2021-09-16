;;; mpdel-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "mpdel" "mpdel.el" (0 0 0 0))
;;; Generated autoloads from mpdel.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "mpdel" '("mpdel-")))

;;;***

;;;### (autoloads nil "mpdel-browser" "mpdel-browser.el" (0 0 0 0))
;;; Generated autoloads from mpdel-browser.el

(autoload 'mpdel-browser-open "mpdel-browser" "\
Open the top level MPDel browser buffer." t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "mpdel-browser" '("mpdel-browser-")))

;;;***

;;;### (autoloads nil "mpdel-core" "mpdel-core.el" (0 0 0 0))
;;; Generated autoloads from mpdel-core.el

(autoload 'mpdel-core-open-artists "mpdel-core" "\
Display all artists in the MPD database." t nil)

(autoload 'mpdel-core-open-stored-playlists "mpdel-core" "\
Display all stored playlists in MPD." t nil)

(autoload 'mpdel-core-open-albums "mpdel-core" "\
Display all albums in the MPD database." t nil)

(autoload 'mpdel-core-open-directories "mpdel-core" "\
Display all top-level directories in the MPD database." t nil)

(autoload 'mpdel-core-search-by-artist "mpdel-core" "\
Display all songs whose artist's name match NAME.
Interactively, ask for NAME.

\(fn NAME)" t nil)

(autoload 'mpdel-core-search-by-album "mpdel-core" "\
Display all songs whose album's name match NAME.
Interactively, ask for NAME.

\(fn NAME)" t nil)

(autoload 'mpdel-core-search-by-title "mpdel-core" "\
Display all songs matching TITLE.
Interactively, ask for TITLE.

\(fn TITLE)" t nil)

(autoload 'mpdel-core-search-by-filter "mpdel-core" "\
Display all songs matching the mpd filter expression FILTER.
Interactively, ask for FILTER.

Example: ((artist == 'name') AND (any contains 'text'))
Documentation: https://www.musicpd.org/doc/html/protocol.html#filters

\(fn FILTER)" t nil)

(autoload 'mpdel-core-volume-increase "mpdel-core" "\
Increase the playback volume by AMOUNT.
If AMOUNT is nil, `mpdel-core-volume-step' is used instead.
Called interactively, AMOUNT can be passed as a prefix argument.

\(fn &optional AMOUNT)" t nil)

(autoload 'mpdel-core-volume-decrease "mpdel-core" "\
Decrease the playback volume by AMOUNT.
If AMOUNT is nil, `mpdel-core-volume-step' is used instead.
Called interactively, AMOUNT can be passed as a prefix argument.

\(fn &optional AMOUNT)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "mpdel-core" '("mpdel-core-")))

;;;***

;;;### (autoloads nil "mpdel-playlist" "mpdel-playlist.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from mpdel-playlist.el

(autoload 'mpdel-playlist-open "mpdel-playlist" "\
Display the current playlist." t nil)

(autoload 'mpdel-playlist-open-stored-playlist "mpdel-playlist" "\
Ask for a stored playlist and open it." t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "mpdel-playlist" '("mpdel-playlist-")))

;;;***

;;;### (autoloads nil "mpdel-song" "mpdel-song.el" (0 0 0 0))
;;; Generated autoloads from mpdel-song.el

(autoload 'mpdel-song-open "mpdel-song" "\
Open a buffer to display information about SONG.
If SONG is nil, use current song instead.

When SONG is nil, the buffer updates itself to keep showing
latest song.  Additionally, the buffer lets the user control
playback.

\(fn &optional SONG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "mpdel-song" '("mpdel-song-")))

;;;***

;;;### (autoloads nil "mpdel-tablist" "mpdel-tablist.el" (0 0 0 0))
;;; Generated autoloads from mpdel-tablist.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "mpdel-tablist" '("mpdel-tablist-")))

;;;***

;;;### (autoloads nil nil ("mpdel-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; mpdel-autoloads.el ends here
