;;; libmpdel.el --- Communication with an MPD server  -*- lexical-binding: t; -*-

;; Copyright (C) 2017-2021  Damien Cassou

;; Author: Damien Cassou <damien@cassou.me>
;; Keywords: multimedia
;; Url: https://gitea.petton.fr/mpdel/libmpdel
;; Package-requires: ((emacs "25.1"))
;; Version: 1.3.0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; The package libmpdel is an Emacs library client to communicate with
;; Music Player Daemon (MPD), a flexible, powerful, server-side
;; application for playing music.  For a user interface, please check
;; the mpdel project instead (which depends on this one).

;;; Code:
(require 'time-stamp)
(require 'tq)
(require 'cl-lib)
(require 'subr-x)


;;; Customization

(defgroup libmpdel nil
  "Communication with an MPD server."
  :group 'comm)

(defcustom libmpdel-hostname "localhost"
  "MPD server location to connect to.  Also see `libmpdel-port'.
If this string starts with a slash, it means connect to a local
Unix socket with such absolute filename.  Please see the MPD
server documentation for server configuration info.

The advantage of such a setup is that file and/or directory
permission modes can be used to enforce access control,
without the need for a password."
  :type 'string)

(defcustom libmpdel-port 6600
  "MPD server port to connect to.  Also see `libmpdel-hostname'."
  :type 'integer)

(defcustom libmpdel-family 'ipv4
  "MPD address family when connecting via TCP connections.

For more information see `libmpdel-hostname'."
  :type '(choice (const :tag "IPv4" ipv4)
                 (const :tag "IPv6" ipv6)))

(defcustom libmpdel-profiles (list (list "Local server" libmpdel-hostname libmpdel-port libmpdel-family))
  "List of (NAME HOST PORT . FAMILY) when using several MPD servers."
  :type '(repeat (list
                  :tag "Profile"
                  :value ("Local server" "localhost" 6600)
                  (string :tag "name")
                  (string :tag "host")
                  (integer :tag "port")
                  (choice (const :tag "IPv4" ipv4)
                          (const :tag "IPv6" ipv6)))))

(defcustom libmpdel-music-directory "~/Music"
  "MPD `music_directory' variable's value.

This is used to map MPD's music files to the file-system."
  :type 'directory)

(defcustom libmpdel-current-playlist-changed-hook nil
  "Functions to call when the current playlist is modified."
  :type 'hook
  :group 'libmpdel)

(defcustom libmpdel-stored-playlist-changed-hook nil
  "Functions to call when a stored playlist is modified."
  :type 'hook
  :group 'libmpdel)

(defcustom libmpdel-player-changed-hook nil
  "Functions to call when the player status changes.
This includes starting, stopping and seeking music."
  :type 'hook
  :group 'libmpdel)

(defcustom libmpdel-current-song-changed-hook nil
  "Functions to call when the current song changes.
See `libmpdel-current-song-id'."
  :type 'hook
  :group 'libmpdel)


;;; Global private variables

(defvar libmpdel--connection nil
  "Current connection to the MPD server.
The logs of this connection are accessible in the `*mpd*' buffer.")

(defconst libmpdel--response-regexp
  (rx line-start
      (or
       (and "OK" (? " MPD " (one-or-more not-newline)))
       (and "ACK ["
            (one-or-more (any digit)) "@" (one-or-more (any digit))
            "] " (one-or-more not-newline)))
      "\n")
  "Regexp matching the responses sent by the MPD server.")

(defconst libmpdel--msgfield-regexp
  (rx line-start
      (group (+? (not (any ?:))))
      ": "
      (group (* not-newline))
      line-end)
  "Regexp matching a line consisting of a key and a value.
The key is stored in group 1 and the value in group 2.")

(defvar libmpdel--msghandlers nil
  "Current commands sent to the server.
Each element in the list is of the form (COMMAND HANDLER BUFFER).

COMMAND is the query sent to the server.  Even though this
information is not necessary, it is useful to better understand
the log.

HANDLER is a function executed when the answers to COMMAND comes
back.  The function must accept one parameter (usually named
MESSAGE) that will contain the answer.

BUFFER is a buffer that was active when COMMAND was sent.  This
buffer is made active again while executing HANDLER.

An invariant of this MPD client is that there is always an IDLE
command sent to the server (and its corresponding handler in this
variable).  This means our client is always registered to
notifications in the server.  When we want to send a command to
the server (for example to change the current song), we always
have to (1) cancel the IDLE first (with a \"noidle\"
command), (2) send the command we want, and (3) send the IDLE
command again.  Canceling the current \"idle\" command is done
in `mpdel-send-command'.  Sending \"idle\" again is done in the
handler for \"idle\" that will be triggered when the empty answer
for the canceled \"idle\" arrives.

Because MPD answers in the order the commands are sent, we know
that the first handler is the one to execute when we receive a
message from the server.")


;;; Data structures

(cl-defstruct (libmpdel-artist
               (:constructor libmpdel--artist-create)
               (:conc-name libmpdel--artist-))
  (name nil :read-only t))

(cl-defstruct (libmpdel-album
               (:constructor libmpdel--album-create)
               (:conc-name libmpdel--album-))
  (name nil :read-only t)
  (artist nil :read-only t))

(cl-defstruct (libmpdel-song
               (:constructor libmpdel--song-create)
               (:conc-name libmpdel--song-))
  (name nil :read-only t)
  (track nil :read-only t)
  (file nil :read-only t)
  (album nil :read-only t)
  (disc nil :read-only t)
  (date nil :read-only t)
  (id nil :read-only t)
  (pos nil :read-only t))

(cl-defstruct (libmpdel-stored-playlist
               (:constructor libmpdel--stored-playlist-create)
               (:conc-name libmpdel--stored-playlist-))
  (name nil :read-only t))

(cl-defstruct (libmpdel-search-criteria
               (:constructor libmpdel-search-criteria-create)
               (:conc-name libmpdel--search-criteria-))
  (type nil :read-only t)
  (what nil :read-only t))

(cl-defstruct (libmpdel-filter
               (:constructor libmpdel-filter-create)
               (:conc-name libmpdel--filter-))
  (text nil :read-only t))

(defun libmpdel-artist-name (entity)
  "Return artist name of ENTITY."
  (libmpdel--artist-name (libmpdel-artist entity)))

(cl-defgeneric libmpdel-artist (entity)
  "Return artist of ENTITY.")

(cl-defmethod libmpdel-artist ((artist libmpdel-artist))
  "Return ARTIST."
  artist)

(cl-defmethod libmpdel-artist ((album libmpdel-album))
  "Return the ALBUM's artist."
  (libmpdel--album-artist album))

(cl-defmethod libmpdel-artist ((song libmpdel-song))
  "Return the SONG's artist."
  (libmpdel-artist (libmpdel--song-album song)))

(defun libmpdel-album-name (entity)
  "Return album name of ENTITY."
  (libmpdel--album-name (libmpdel-album entity)))

(cl-defgeneric libmpdel-album (entity)
  "Return album of ENTITY.")

(cl-defmethod libmpdel-album ((album libmpdel-album))
  "Return ALBUM."
  album)

(cl-defmethod libmpdel-album ((song libmpdel-song))
  "Return SONG's album."
  (libmpdel--song-album song))

(cl-defgeneric libmpdel-entity-name (entity)
  "Return the name of ENTITY.")

(cl-defmethod libmpdel-entity-name ((artist libmpdel-artist))
  "Return ARTIST's name."
  (libmpdel--artist-name artist))

(cl-defmethod libmpdel-entity-name ((album libmpdel-album))
  "Return ALBUM's name."
  (libmpdel--album-name album))

(cl-defmethod libmpdel-entity-name ((song libmpdel-song))
  "Return SONG's name.

If the SONG's name is nil, return the filename instead."
  (or (libmpdel--song-name song)
      (libmpdel--song-file song)))

(cl-defmethod libmpdel-entity-name ((_entity (eql stored-playlists)))
  "Return a string describing the `stored-playlists' entity."
  "Stored playlists")

(cl-defmethod libmpdel-entity-name ((_entity (eql artists)))
  "Return a string describing the `artists' entity."
  "All artists")

(cl-defmethod libmpdel-entity-name ((_entity (eql albums)))
  "Return a string describing the `albums' entity."
  "All albums")

(cl-defmethod libmpdel-entity-name ((_entity (eql current-playlist)))
  "Return a string describing the `current-playlist' entity."
  "Current playlist")

(cl-defmethod libmpdel-entity-name ((stored-playlist libmpdel-stored-playlist))
  "Return STORED-PLAYLIST's name."
  (libmpdel--stored-playlist-name stored-playlist))

(cl-defmethod libmpdel-entity-name ((search-criteria libmpdel-search-criteria))
  "Return a string representing SEARCH-CRITERIA."
  (format "search %s: \"%s\""
          (libmpdel--search-criteria-type search-criteria)
          (libmpdel--search-criteria-what search-criteria)))

(cl-defmethod libmpdel-entity-name ((filter libmpdel-filter))
  "Return a string representing FILTER."
  (format "filter %s" (libmpdel--filter-text filter)))

(cl-defgeneric libmpdel-entity-parent (_entity)
  "Return parent of ENTITY, nil if none."
  nil)

(cl-defmethod libmpdel-entity-parent ((song libmpdel-song))
  "Return SONG's album."
  (libmpdel-album song))

(cl-defmethod libmpdel-entity-parent ((album libmpdel-album))
  "Return ALBUM's artist."
  (libmpdel-artist album))

(cl-defmethod libmpdel-entity-parent ((_artist libmpdel-artist))
  "Return the `artists' entity."
  'artists)

(cl-defmethod libmpdel-entity-parent ((_stored-playlist libmpdel-stored-playlist))
  "Return the `stored-playlists' entity."
  'stored-playlists)

(cl-defgeneric libmpdel-entity-id (entity)
  "Return an identifier string for ENTITY."
  entity)

(cl-defmethod libmpdel-entity-id ((song libmpdel-song))
  "Return the SONG's filename."
  ;; Override of default implementation to ignore changing ids and
  ;; position.
  (libmpdel--song-file song))

(defun libmpdel-song-file (song)
  "Return the filename of SONG."
  (libmpdel--song-file song))

(defun libmpdel-song-track (song)
  "Return the track number of SONG within its album."
  (or (libmpdel--song-track song) ""))

(defun libmpdel-entity-date (song)
  "Return the date of SONG."
  (or (libmpdel--song-date song) ""))

(defun libmpdel-song-disc (song)
  "Return the disc number of SONG within its album."
  (or (libmpdel--song-disc song) ""))

(defun libmpdel-song-id (song)
  "Return SONG id within the current playlist, nil if none."
  (libmpdel--song-id song))

(defun libmpdel-song-position (song)
  "Return position of SONG in playlist, nil if not in playlist."
  (let ((pos (libmpdel--song-pos song)))
    (when (and (stringp pos) (not (string= pos "")))
      (string-to-number pos))))

(defun libmpdel--create-song-from-data (song-data)
  "Return a song from SONG-DATA, a server's response."
  (libmpdel--song-create
   :name (cdr (assq 'Title song-data))
   :track (cdr (assq 'Track song-data))
   :file (cdr (assq 'file song-data))
   :album (libmpdel--album-create
           :name (cdr (assq 'Album song-data))
           :artist (libmpdel--artist-create :name (cdr (assq 'Artist song-data))))
   :date (cdr (assq 'Date song-data))
   :disc (cdr (assq 'Disc song-data))
   :id (cdr (assq 'Id song-data))
   :pos (cdr (assq 'Pos song-data))))

(defun libmpdel--create-songs-from-data (data)
  "Return a list of songs from DATA, a server's response."
  (mapcar #'libmpdel--create-song-from-data (libmpdel-group-data data)))

(defun libmpdel-current-playlist-p (entity)
  "Return non-nil if ENTITY is the current playlist."
  (eq entity 'current-playlist))


;;; Helper functions

(defun libmpdel--process ()
  "Return the process communicating with the MPD server."
  (tq-process libmpdel--connection))

(defun libmpdel--process-buffer ()
  "Return the buffer associated with the connection process."
  (process-buffer (libmpdel--process)))

(defsubst libmpdel--connection-address-local-p ()
  "Return non-nil if the MPD server address is a local family address."
  (eq ?/ (aref libmpdel-hostname 0)))

(defsubst libmpdel--open-stream ()
  "Open and return connection to the MPD process."
  (if (not (libmpdel--connection-address-local-p))
      (make-network-process
       :name "mpd"
       :buffer "*mpd*"
       :host libmpdel-hostname
       :service libmpdel-port
       :family libmpdel-family
       :type nil)
    (make-network-process
     :name "mpd" :buffer "*mpd*"
     :family 'local :service  libmpdel-hostname)))

(defun libmpdel--connect ()
  "Create a new connection with the MPD server."
  ;; The *mpd* buffer will contain all the communication logs
  (when (libmpdel-connected-p)
    (user-error "A connection is already opened"))
  (with-current-buffer (get-buffer-create "*mpd*")
    (setq-local buffer-read-only t)
    (let ((inhibit-read-only t))
      (erase-buffer)))
  (setq libmpdel--connection (tq-create (libmpdel--open-stream)))
  (set-process-coding-system (libmpdel--process) 'utf-8-unix 'utf-8-unix)
  (set-process-query-on-exit-flag (libmpdel--process) nil)
  ;; Take care of the initial welcome message from server that we
  ;; don't ask for:
  (setq libmpdel--msghandlers '(("welcome" libmpdel--msghandler-ignore nil)))
  (tq-queue-add libmpdel--connection nil libmpdel--response-regexp nil #'libmpdel--message-filter)
  (libmpdel-refresh-status)
  ;; As an invariant of the MPD client, there is always an "idle"
  ;; command sent to the server. This acts like a registration to the
  ;; server's notifications. See `libmpdel--msghandlers' for more
  ;; information.
  (libmpdel--raw-send-command-with-handler "idle" #'libmpdel--msghandler-idle))

;;;###autoload
(defun libmpdel-connect-profile (profile)
  "Connect to MPD server defined in PROFILE.
Interactively, let the user choose PROFILE from `libmpdel-profiles'.

If a connection already exists, terminate it first."
  (interactive (list (libmpdel--select-profile)))
  (let* ((libmpdel-hostname (cl-second profile))
         (libmpdel-port (cl-third profile))
         (libmpdel-family (cl-fourth profile)))
    (when (libmpdel-connected-p)
      (libmpdel-disconnect))
    (libmpdel--connect)))

(defun libmpdel--raw-send-command (command)
  "Send COMMAND, a string, to the server and log that."
  (libmpdel--log command "->")
  (tq-enqueue
   libmpdel--connection
   (format "%s\n" command)
   libmpdel--response-regexp
   nil
   #'libmpdel--message-filter))

(defun libmpdel--raw-send-command-with-handler (command &optional handler)
  "Send COMMAND to MPD server and set HANDLER for the response.
If HANDLER is nil, response will be ignored.

If command is a string, send that.  Otherwise, it must be a list
that will be passed to `format' before being sent."
  (let ((command (if (listp command)
                     (apply #'format command)
                   command)))
    (setq libmpdel--msghandlers
          (append libmpdel--msghandlers
                  `((,command
                     ,(or handler #'libmpdel--msghandler-ignore)
                     ,(current-buffer)))))
    (libmpdel--raw-send-command command)))

(defun libmpdel--message-filter (_ message)
  "Take care of the MESSAGE sent by the server.

The first parameter is ignored.  MESSAGE contains a string
representing the answer from the server."
  ;; Because errors in handlers are not raised by Emacs, we log them.
  (condition-case-unless-debug error
      (progn
        ;; because answers arrive in the same order we sent the
        ;; commands, we are sure that the first handler is the one to
        ;; use.
        (cl-destructuring-bind (command handler buffer) (pop libmpdel--msghandlers)
          (libmpdel--log (format "\"%s\" (as answer to \"%s\")" message command)
                         "<-")
          ;; if answer is a ACK, then there was a problem. We log it as such.
          (if (string= (substring message 0 3) "ACK")
              (libmpdel--log "ACK message" "ko")
            (with-current-buffer (if (buffer-live-p buffer) buffer (current-buffer))
              (funcall handler (libmpdel--extract-data message))))))
    (error (libmpdel--log error "ko"))))

(defun libmpdel--log (string type-string)
  "Add STRING at end of *mpd* buffer.

TYPE-STRING is a two-letter string classifying the kind of
message to log."
  (with-current-buffer (libmpdel--process-buffer)
    (let ((inhibit-read-only t)
          (moving (= (point) (process-mark (libmpdel--process)))))
      (save-excursion
        ;; Insert the text, advancing the process marker.
        (goto-char (process-mark (libmpdel--process)))
        (insert "-------------------------\n")
        (insert (format "%s [%s] %s\n" type-string (time-stamp-string) string))
        (set-marker (process-mark (libmpdel--process)) (point)))
      (if moving (goto-char (process-mark (libmpdel--process)))))))

(defun libmpdel--msghandler-idle (data)
  "Handler for the response DATA to the \"idle\" command.
This handler is responsible for sending another \"idle\"
command."
  ;; Because "idle" only informs about what changed (e.g., "the
  ;; playback state changed") without telling the new state (e.g.,
  ;; "the player is now stopped"), we have to ask for the details:
  (when data
    (libmpdel-refresh-status))
  ;; Each time an "idle" is finished, we start a new one:
  (libmpdel--raw-send-command-with-handler "idle" #'libmpdel--msghandler-idle)
  (mapc (lambda (changed-subsystem)
          (cl-case (intern (cdr changed-subsystem))
            ;; At this point, libmpdel has only been informed that
            ;; something changed (e.g., "the current playlist has been
            ;; changed"). We don't have the details (e.g., "the
            ;; current playlist contains these songs").  As a result,
            ;; hook functions will have to fetch the details by
            ;; themselves if they need to.  On the contrary, for hook
            ;; functions requiring libmpdel to have new data, use
            ;; `libmpdel--msghandler-status'.
            (playlist (run-hooks 'libmpdel-current-playlist-changed-hook))
            (stored_playlist (run-hooks 'libmpdel-stored-playlist-changed-hook))))
        data))

(defun libmpdel--msghandler-status (data)
  "Handler for the response DATA to the \"status\" command."
  (dolist (status-pair data)
    (let ((status-key (car status-pair))
          (status-value (cdr status-pair)))
      (cl-case status-key
        (state (libmpdel--set-play-state status-value))
        (songid (libmpdel--set-current-song status-value))
        (playlistlength (libmpdel--set-playlist-length status-value))
        (volume (libmpdel--set-volume status-value))
        (random (libmpdel--set-random status-value))
        (repeat (libmpdel--set-repeat status-value))
        (single (libmpdel--set-single status-value)))))
  ;; When no song is being played, 'songid is not in DATA.  If that's
  ;; the case, we have to set current song to nil:
  (unless (cl-member 'songid data :key #'car)
    (libmpdel--set-current-song nil)))

(defun libmpdel--msghandler-ignore (_)
  "No handler was associated to last response."
  ;; nothing to do
  nil)

(defun libmpdel--extract-data (message)
  "Return MESSAGE."
  (save-match-data
    (with-temp-buffer
      (insert message)
      (let ((end-of-message (point-at-bol))
            (data nil))
        (goto-char (point-min))
        (while (re-search-forward libmpdel--msgfield-regexp end-of-message t)
          (push (cons (intern (match-string 1)) (match-string 2)) data))
        (reverse data)))))

(defun libmpdel--string<-ignore-case (str1 str2)
  "Compare the contents of STR1 and STR2, ignoring case."
  (let ((comp (compare-strings str1 nil nil str2 nil nil t)))
    (or (eq comp t) (< comp 0))))

(defmacro libmpdel--define-state (name value-desc &rest set-body)
  "Generate code to set and get state for NAME.

Name is a symbol (e.g., `volume' or `play-state') naming the
state to generate code for.

VALUE-DESC is a string describing the kind of value accepted for
this state.

SET-BODY is a list of forms to put in the generated setter
function.  During execution of SET-BODY, a variable NEW-VALUE is
bound containing the value to set."
  (declare (indent 1))
  `(progn
     (defvar ,(intern (format "libmpdel--%s" name)) nil
       ,(format "Current %s of MPD server.\n%s" name value-desc))

     (defun ,(intern (format "libmpdel--set-%s" name)) (new-value)
       ,(format "Save NEW-VALUE as current %s.\n%s" name value-desc)
       ,@set-body)

     (defun ,(intern (format "libmpdel-%s" name)) ()
       ,(format "Return current value of %s.\n%s" name value-desc)
       ,(intern (format "libmpdel--%s" name)))))

(libmpdel--define-state play-state
  "Value is `play', `pause' or `stop'."
  (let ((new-state (intern new-value))
        (old-state libmpdel--play-state))
    (unless (equal old-state new-state)
      (setq libmpdel--play-state new-state)
      (run-hooks 'libmpdel-player-changed-hook))))

(defun libmpdel-playing-p ()
  "Return non-nil if player is playing, nil otherwise."
  (eq 'play (libmpdel-play-state)))

(defun libmpdel-paused-p ()
  "Return non-nil if player is paused, nil otherwise."
  (eq 'pause (libmpdel-play-state)))

(defun libmpdel-stopped-p ()
  "Return non-nil if player is stopped, nil otherwise."
  (eq 'stop (libmpdel-play-state)))

(libmpdel--define-state current-song
  "An entity representing currently played song."
  (when (libmpdel--new-current-song-p new-value)
    (libmpdel-send-command
     "currentsong"
     (lambda (data)
       (setq libmpdel--current-song (and data (libmpdel--create-song-from-data data)))
       (run-hooks 'libmpdel-current-song-changed-hook)))))

(defun libmpdel--new-current-song-p (song-id)
  "Return non-nil if SONG-ID differs from `libmpdel--current-song'."
  (let ((current-song-id (and libmpdel--current-song (libmpdel-song-id libmpdel--current-song))))
    (not (equal song-id current-song-id))))

(libmpdel--define-state playlist-length
  "Number of songs in current playlist."
  (setq libmpdel--playlist-length (string-to-number new-value)))

(libmpdel--define-state volume
  "Value is a string representing a number between 0 and 100."
  (setq libmpdel--volume new-value))

(libmpdel--define-state random
  "Boolean indicating if songs are played randomly or in order."
  (setq libmpdel--random (string= new-value "1")))

(libmpdel--define-state repeat
  "Boolean indicating if current playlist or song is repeated after it ends."
  (setq libmpdel--repeat (string= new-value "1")))

(libmpdel--define-state single
  "Symbol indicating if current song is repeated `forever', only `once' or `never'."
  (setq libmpdel--single
        (cond
         ((string= new-value "oneshot") 'once)
         ((string= new-value "1") 'forever)
         (t 'never))))

(defun libmpdel-time-to-string (time)
  "Return a string representing TIME, a number in a string."
  (if (not time)
      "0"
    (let* ((time (string-to-number time))
           (seconds (mod time 60))
           (minutes (/ (- time seconds) 60)))
      (format "%02d:%02d" (truncate minutes) (truncate seconds)))))

(defun libmpdel-completing-read (prompt entities &optional transformer)
  "PROMPT user to select one entity among ENTITIES.
Return the selected entity.

Transform each entity to a string with TRANSFORMER,
`libmpdel-entity-name' if nil.

The user is allowed to exit by typing a string not matching any
entity.  In this case, the user must confirm and the typed string
is returned."
  (let* ((transformer (or transformer #'libmpdel-entity-name))
         (map (make-hash-table :test 'equal :size (length entities)))
         (entity-strings (mapcar (lambda (entity) (funcall transformer entity)) entities)))
    (cl-mapcar (lambda (entity entity-string)
                 (puthash entity-string entity map))
               entities entity-strings)
    (let ((entity-string (completing-read prompt entity-strings nil 'confirm)))
      (gethash entity-string map entity-string))))

(defun libmpdel-completing-read-entity (function prompt entity &optional transformer)
  "Call FUNCTION after prompting for an element of ENTITY.

Pass PROMPT, the elements of ENTITY and TRANSFORMER to
`libmpdel-completing-read'."
  (libmpdel-list
   entity
   (lambda (entities)
     (funcall function
              (libmpdel-completing-read prompt entities transformer)))))

(defun libmpdel-funcall-on-stored-playlist (function)
  "Pass a stored playlist as parameter to FUNCTION.
The user is asked to choose for a stored playlist first.

The user is allowed to enter a name for a non-existing stored
playlist.  In this case, the user must confirm and the stored
playlist is created before being passed as parameter to
FUNCTION."
  (libmpdel-completing-read-entity
   (lambda (stored-playlist)
     (let ((stored-playlist (if (stringp stored-playlist)
                                (libmpdel--stored-playlist-create :name stored-playlist)
                              stored-playlist)))
       (funcall function stored-playlist)))
   "Stored playlist: "
   'stored-playlists))

(defun libmpdel-current-playlist-add (entity)
  "Add ENTITY to a current playlist.

ENTITY can also be a list of entities to add."
  (libmpdel-playlist-add entity 'current-playlist))

(defun libmpdel-current-playlist-replace (entity)
  "Replace current playlist with ENTITY.

ENTITY can also be a list of entities to replace with."
  (libmpdel-playlist-replace entity 'current-playlist))

(defun libmpdel-stored-playlist-add (entity)
  "Add ENTITY to a stored playlist.
The user is asked to choose for a stored playlist first.

ENTITY can also be a list of entities to add."
  (libmpdel-funcall-on-stored-playlist
   (apply-partially #'libmpdel-playlist-add entity)))

(defun libmpdel-stored-playlist-replace (entity)
  "Replace a stored playlist with ENTITY.
The user is asked to choose for a stored playlist first.

ENTITY can also be a list of entities to replace with."
  (libmpdel-funcall-on-stored-playlist
   (apply-partially #'libmpdel-playlist-replace entity)))

(defun libmpdel-current-playlist-insert (entity)
  "Insert ENTITY after currently-played song and play it.
ENTITY can also be a list of entities in which case all entities
are added and the first one is played."
  (libmpdel-list-songs
   entity
   (lambda (songs)
     (libmpdel-send-commands
      (mapcar (lambda (song) (format "addid %S" (libmpdel-song-file song))) songs)
      (lambda (data)
        (let ((song-ids (mapcar (lambda (song-data) (cdr song-data)) data))
              ;; Add after current song if possible:
              (target-index (if (libmpdel-current-song) "-1" "0")))
          (libmpdel-send-commands
           ;; The reverse is important to get the songs in the same
           ;; order as in the selection:
           (mapcar
            (lambda (song-id) (format "moveid %s %s" song-id target-index))
            (reverse song-ids))
           (lambda (_) (libmpdel-send-command `("playid %s" ,(car song-ids)))))))))))

(defun libmpdel-async-mapcar (list mapfn callback)
  "Apply MAPFN to each element of LIST and pass result to CALLBACK.

MAPFN is a function taking 2 arguments: the element to map and a
callback to call when the mapping is done."
  (if (not list)
      (funcall callback nil)
    (funcall                  ; transform the first element
     mapfn
     (car list)
     (lambda (first-mapped)
       (libmpdel-async-mapcar         ; transform the rest
        (cdr list)
        mapfn
        (lambda (latter-elements)
          (funcall callback
                   (cons first-mapped
                         latter-elements))))))))

(defun libmpdel-async-mapcan (list mapfn callback)
  "Apply MAPFN to each element of LIST.
Concatenate the results and pass that to CALLBACK.

MAPFN is a function taking 2 arguments: the element to map and a
callback to call when the mapping is done."
  (libmpdel-async-mapcar
   list
   mapfn
   (lambda (groups)
     (funcall
      callback
      (apply #'cl-concatenate 'list groups)))))

(defun libmpdel--get-profile-from-name (name)
  "Return an element of `libmpdel-profiles' matching NAME."
  (cl-find name libmpdel-profiles :test #'string= :key #'car))

(defun libmpdel--select-profile ()
  "Ask the user to select a profile among `libmpdel-profiles' and return it."
  (unless (consp libmpdel-profiles)
    (user-error "Add profiles to `libmpdel-profiles'"))
  (if (= 1 (length libmpdel-profiles))
      (progn
        (message "Only 1 profile defined in `libmpdel-profiles'")
        (car libmpdel-profiles))
    (let* ((profile-names (mapcar #'car libmpdel-profiles))
           (profile-name (completing-read "Choose an MPD profile"
                                          profile-names nil t)))
      (libmpdel--get-profile-from-name profile-name))))


;;; Public functions

(defun libmpdel-connected-p ()
  "Return non-nil if there is a connection to MPD server."
  (and libmpdel--connection
       (process-live-p (libmpdel--process))))

(defun libmpdel-ensure-connection ()
  "Make sure there is an active connection to the MPD server."
  (unless (libmpdel-connected-p)
    (libmpdel--connect)))

(defun libmpdel-disconnect ()
  "Close connection to the MPD server."
  (when (not (libmpdel-connected-p))
    (user-error "There is no connection to MPD"))
  (tq-close libmpdel--connection)
  (setq libmpdel--connection nil))

(defun libmpdel-send-command (command &optional handler)
  "Send COMMAND to server and register HANDLER for the answer.
If HANDLER is nil, ignore response."
  (libmpdel-ensure-connection)
  ;; if current command is IDLE, we have to cancel it. See
  ;; `mpdel-msghandlers' for more information.
  (when (eql (elt (car (last libmpdel--msghandlers)) 1) #'libmpdel--msghandler-idle)
    (libmpdel--raw-send-command "noidle"))
  (libmpdel--raw-send-command-with-handler command handler))

(defun libmpdel-send-commands (commands &optional handler)
  "Send several COMMANDS at once and execute HANDLER once with result."
  (libmpdel-send-command
   (with-temp-buffer
     (insert "command_list_begin\n")
     (mapc (lambda (command) (insert command "\n")) commands)
     (insert "command_list_end")
     (buffer-substring-no-properties (point-min) (point-max)))
   handler))

(defun libmpdel-entries (data key)
  "Collect DATA entries matching KEY."
  (mapcar #'cdr (cl-remove-if-not (apply-partially #'eq key) data :key #'car)))

(defun libmpdel-sorted-entries (data key)
  "Sort and collect DATA entries matching KEY."
  (sort (libmpdel-entries data key) #'libmpdel--string<-ignore-case))

(defun libmpdel-group-data (data)
  "Find repeating fields in DATA and group them."
  (when data
    (let ((first-key (caar data))
          result group)
      (mapc (lambda (key-value)
              (when (and
                     (eq (car key-value) first-key)
                     group)
                (push (reverse group) result)
                (setq group nil))
              (push key-value group))
            data)
      (push (reverse group) result)
      (reverse result))))

(cl-defgeneric libmpdel-dired (entity)
  "Open `dired' on ENTITY.")

(eval-when-compile
  (declare-function dired-jump "dired-x"))

(cl-defmethod libmpdel-dired ((song libmpdel-song))
  "Open `dired' on SONG."
  (require 'dired-x)
  (dired-jump t (expand-file-name (libmpdel-song-file song) libmpdel-music-directory)))

(defun libmpdel-equal (entity1 entity2)
  "Return non-nil if ENTITY1 and ENTITY2 represent the same entity."
  (equal (libmpdel-entity-id entity1) (libmpdel-entity-id entity2)))


;;; Helper queries

(cl-defgeneric libmpdel-entity-to-criteria (entity)
  "Return search query matching all songs from ENTITY.")

(cl-defmethod libmpdel-entity-to-criteria ((query string))
  "Return QUERY."
  query)

(cl-defmethod libmpdel-entity-to-criteria ((artist libmpdel-artist))
  "Return search query matching all songs from ARTIST."
  (format "artist %S" (libmpdel-entity-name artist)))

(cl-defmethod libmpdel-entity-to-criteria ((album libmpdel-album))
  "Return search query matching all songs from ALBUM."
  (format "%s album %S"
          (libmpdel-entity-to-criteria (libmpdel-artist album))
          (libmpdel-entity-name album)))

(cl-defmethod libmpdel-entity-to-criteria ((song libmpdel-song))
  "Return search query matching SONG."
  (format "%s title %S"
          (libmpdel-entity-to-criteria (libmpdel-album song))
          (libmpdel-entity-name song)))

(cl-defgeneric libmpdel-list (entity function)
  "Call FUNCTION with all children of ENTITY as parameter."
  (libmpdel-list-songs entity function))

(cl-defmethod libmpdel-list ((_entity (eql artists)) function)
  "Call FUNCTION with all artists as parameter."
  (libmpdel-send-command
   "list artist"
   (lambda (data)
     (funcall function
              (mapcar
               (lambda (artist-name) (libmpdel--artist-create :name artist-name))
               (libmpdel-sorted-entries data 'Artist))))))

(defvar libmpdel--unknown-artist
  (libmpdel--artist-create :name "")
  "An anonymous artist instance.")

(cl-defmethod libmpdel-list ((_entity (eql albums)) function)
  "Call FUNCTION with all albums as parameter."
  (libmpdel-send-command
   "list album"
   (lambda (data)
     (funcall function
              (mapcar
               (lambda (album-name)
                 (libmpdel--album-create :name album-name
                                         :artist libmpdel--unknown-artist))
               (libmpdel-sorted-entries data 'Album))))))

(cl-defmethod libmpdel-list ((_entity (eql stored-playlists)) function)
  "Call FUNCTION with all stored playlists as parameter."
  (libmpdel-send-command
   "listplaylists"
   (lambda (data)
     (funcall function
              (mapcar
               (lambda (playlist-name) (libmpdel--stored-playlist-create :name playlist-name))
               (libmpdel-sorted-entries data 'playlist))))))

(cl-defmethod libmpdel-list ((artist libmpdel-artist) function)
  "Call FUNCTION with all albums of ARTIST as parameter."
  (libmpdel-send-command
   `("list album %s" ,(libmpdel-entity-to-criteria artist))
   (lambda (data)
     (funcall function
              (mapcar
               (lambda (album-name) (libmpdel--album-create :name album-name :artist artist))
               (libmpdel-sorted-entries data 'Album))))))

(cl-defgeneric libmpdel-list-songs (entity function)
  "Call FUNCTION with all songs of ENTITY."
  (libmpdel-send-command
   `("find %s" ,(libmpdel-entity-to-criteria entity))
   (lambda (data)
     (funcall function (libmpdel--create-songs-from-data data)))))

(cl-defmethod libmpdel-list-songs ((stored-playlist libmpdel-stored-playlist) function)
  "Call FUNCTION with all songs of STORED-PLAYLIST."
  (libmpdel-send-command
   `("listplaylistinfo %S" ,(libmpdel-entity-name stored-playlist))
   (lambda (data)
     (funcall function (libmpdel--create-songs-from-data data)))))

(cl-defmethod libmpdel-list-songs ((_ (eql current-playlist)) function)
  "Call FUNCTION with all songs of the current playlist."
  (libmpdel-send-command
   "playlistinfo"
   (lambda (data)
     (funcall function (libmpdel--create-songs-from-data data)))))

(cl-defmethod libmpdel-list-songs ((search-criteria libmpdel-search-criteria) function)
  "Call FUNCTION with all songs matching SEARCH-CRITERIA."
  (libmpdel-send-command
   `("search %s %S"
     ,(libmpdel--search-criteria-type search-criteria)
     ,(libmpdel--search-criteria-what search-criteria))
   (lambda (data)
     (funcall function (libmpdel--create-songs-from-data data)))))

(cl-defmethod libmpdel-list-songs ((filter libmpdel-filter) function)
  "Call FUNCTION with all songs matching FILTER."
  (libmpdel-send-command
   `("search %S" ,(libmpdel--filter-text filter))
   (lambda (data)
     (funcall function (libmpdel--create-songs-from-data data)))))

(cl-defmethod libmpdel-list-songs ((song libmpdel-song) function)
  "Call FUNCTION with SONG."
  (funcall function (list song)))

(cl-defmethod libmpdel-list-songs ((entities list) function)
  "Call FUNCTION with all songs in ENTITIES."
  (libmpdel-async-mapcan entities #'libmpdel-list-songs function))


;;; Playlist queries

(cl-defgeneric libmpdel-playlist-add (entity playlist)
  "Add ENTITY to PLAYLIST.
ENTITY can also be a list of entities to add.")

(cl-defmethod libmpdel-playlist-add (entity (_ (eql current-playlist)))
  "Add ENTITY to the current playlist."
  (let ((id (libmpdel-entity-id entity)))
    (libmpdel-send-command
     (if (and (stringp id) (not (string-empty-p id)))
         `("addid %S" ,id)
       `("findadd %s" ,(libmpdel-entity-to-criteria entity))))))

(cl-defmethod libmpdel-playlist-add (entity (stored-playlist libmpdel-stored-playlist))
  "Add ENTITY to STORED-PLAYLIST."
  (libmpdel-send-command
   `("searchaddpl %S %s"
     ,(libmpdel-entity-name stored-playlist)
     ,(libmpdel-entity-to-criteria entity))))

(cl-defmethod libmpdel-playlist-add ((stored-playlist libmpdel-stored-playlist) (_ (eql current-playlist)))
  "Add content of STORED-PLAYLIST to the current playlist."
  (libmpdel-send-command `("load %S" ,(libmpdel-entity-name stored-playlist))))

(cl-defmethod libmpdel-playlist-add ((entities list) playlist)
  "Add all ENTITIES to PLAYLIST."
  (mapcar (lambda (entity) (libmpdel-playlist-add entity playlist))
          entities))

(defun libmpdel-playlist-replace (entity playlist)
  "Clear PLAYLIST and add ENTITY to it."
  (libmpdel-playlist-clear playlist)
  (libmpdel-playlist-add entity playlist))

(cl-defgeneric libmpdel-playlist-clear (playlist)
  "Remove all songs from PLAYLIST.")

(cl-defmethod libmpdel-playlist-clear ((_ (eql current-playlist)))
  "Remove all songs from the current playlist."
  (libmpdel-send-command "clear"))

(cl-defmethod libmpdel-playlist-clear ((stored-playlist libmpdel-stored-playlist))
  "Remove all songs from STORED-PLAYLIST."
  (libmpdel-send-command `("playlistclear %S" ,(libmpdel-entity-name stored-playlist))))

(cl-defgeneric libmpdel-playlist-delete (songs playlist)
  "Remove SONGS from PLAYLIST.")

(cl-defmethod libmpdel-playlist-delete (songs (_ (eql current-playlist)))
  "Remove SONGS from the current playlist."
  (libmpdel-send-commands
   (mapcar (lambda (song) (format "deleteid %s" (libmpdel-song-id song)))
           songs)))

(cl-defmethod libmpdel-playlist-delete (songs (stored-playlist libmpdel-stored-playlist))
  "Remove SONGS from STORED-PLAYLIST."
  (libmpdel-list
   stored-playlist
   (lambda (all-playlist-songs)
     (let ((song-positions (cl-sort (mapcar (lambda (song)
                                              (cl-position song all-playlist-songs :test #'equal))
                                            songs)
                                    #'>)))
       (libmpdel-send-commands
        (mapcar
         (lambda (song-position)
           (format "playlistdelete %S %s"
                   (libmpdel-entity-name stored-playlist)
                   song-position))
         song-positions))))))

(defun libmpdel-stored-playlists-delete (stored-playlists)
  "Remove STORED-PLAYLISTS."
  (libmpdel-send-commands
   (mapcar
    (lambda (s)
      (format "rm %S" (libmpdel-entity-name s)))
    stored-playlists)))

(defun libmpdel-playlist-move-up (songs)
  "Move up SONGS in current playlist."
  ;; We should move up from first in playlist to last
  (let* ((songs (cl-sort (cl-copy-seq songs) #'< :key #'libmpdel-song-position)))
    ;; Don't move up if first song is selected
    (unless (= (libmpdel-song-position (car songs)) 0)
      (libmpdel-send-commands
       (mapcar (lambda (song)
                 (format "moveid %s %s" (libmpdel-song-id song) (1- (libmpdel-song-position song))))
               songs)))))

(defun libmpdel-playlist-move-down (songs)
  "Move down SONGS in current playlist."
  ;; We should move down from last in playlist to first
  (let* ((songs (cl-sort (cl-copy-seq songs) #'> :key #'libmpdel-song-position)))
    ;; Don't move down if last song is selected
    (unless (= (libmpdel-song-position (car songs)) (1- libmpdel--playlist-length))
      (libmpdel-send-commands
       (mapcar (lambda (song)
                 (format "moveid %s %s" (libmpdel-song-id song) (1+ (libmpdel-song-position song))))
               songs)))))

(defun libmpdel-playlist-save (name)
  "Save current playlist as new stored playlist named NAME."
  (interactive (list (read-from-minibuffer "Enter a new playlist name: ")))
  (libmpdel-send-command
   `("save %S" ,name)
   (lambda (_data) (message "Current playlist saved to %S" name))))


;;; Playback queries

;;;###autoload
(defun libmpdel-playback-set-volume (volume)
  "Set volume to VOLUME."
  (interactive (list
                (read-string (format "Current volume is %s. New volume [0-100]: "
                                     (libmpdel-volume)))))
  (libmpdel-send-command `("setvol %s" ,volume)))

;;;###autoload
(defun libmpdel-playback-next ()
  "Play next song in the playlist."
  (interactive)
  (libmpdel-send-command "next"))

;;;###autoload
(defun libmpdel-playback-previous ()
  "Play previous song in the playlist."
  (interactive)
  (libmpdel-send-command "previous"))

;;;###autoload
(defun libmpdel-play ()
  "Start playing."
  (interactive)
  (libmpdel-send-command "play"))

;;;###autoload
(defun libmpdel-stop ()
  "Stop playing.  See also `libmpdel-playback-play-pause'."
  (interactive)
  (libmpdel-send-command "stop"))

(defun libmpdel-play-song (song)
  "Start playing SONG."
  (let ((song-id (libmpdel-song-id song)))
    (if song-id
        (libmpdel-send-command `("playid %s" ,song-id))
      (libmpdel-current-playlist-insert song))))

;;;###autoload
(defun libmpdel-playback-play-pause ()
  "Toggle between play and pause.
See also `libmpdel-playback-stop'."
  (interactive)
  (libmpdel-send-command
   (cl-case libmpdel--play-state
     (play "pause 1")
     (pause "pause 0")
     (stop "play"))))

;;;###autoload
(defun libmpdel-playback-seek (time &optional handler)
  "Seeks to the position TIME within the current song.

TIME is a string indicating a number of seconds, fractions
allowed.  If prefixed by + or -, then the time is relative to
the current playing position.

If HANDLER is non-nil, execute it with no parameter when seek
succeeds."
  (interactive (list (read-string "New position (e.g., 67, -23, +12): ")
                     (lambda (_) (message "Seek done."))))
  (libmpdel-send-command
   `("seekcur %S" ,time)
   (when handler (lambda (_) (funcall handler)))))

;;;###autoload
(defun libmpdel-playback-set-random ()
  "Set playback mode to random."
  (interactive)
  (libmpdel-send-command `("random 1")))

;;;###autoload
(defun libmpdel-playback-unset-random ()
  "Set playback mode to sequential (not random)."
  (interactive)
  (libmpdel-send-command `("random 0")))

;;;###autoload
(defun libmpdel-playback-set-repeat ()
  "Set playback mode to repeat."
  (interactive)
  (libmpdel-send-command `("repeat 1")))

;;;###autoload
(defun libmpdel-playback-unset-repeat ()
  "Set playback mode to sequential (not repeat)."
  (interactive)
  (libmpdel-send-command `("repeat 0")))

;;;###autoload
(defun libmpdel-playback-set-single-forever ()
  "Set playback single mode to forever.
This will repeat the current song forever."
  (interactive)
  (libmpdel-send-command `("single 1")))

;;;###autoload
(defun libmpdel-playback-set-single-never ()
  "Set playback single mode to never.
This will not repeat the current song."
  (interactive)
  (libmpdel-send-command `("single 0")))

;;;###autoload
(defun libmpdel-playback-set-single-once ()
  "Set playback single mode to once.
This will repeat the current song only once and then keep playing
the current playlist."
  (interactive)
  (libmpdel-send-command `("single oneshot")))


;;; Status queries

(defun libmpdel-refresh-status ()
  "Ask the server for its current status."
  (libmpdel-send-command "status" #'libmpdel--msghandler-status))


;;; Database queries

;;;###autoload
(defun libmpdel-database-update (&optional uri)
  "Update the music database for URI, everything if nil.
Updates the music database: find new files, remove deleted files,
update modified files.

URI is a particular directory or song/file to update.  If you do
not specify it, everything is updated."
  (interactive "i")
  (libmpdel-send-command
   (if uri `("update %S" ,uri) "update")))

(provide 'libmpdel)
;;; libmpdel.el ends here

;; Local Variables:
;; checkdoc-arguments-in-order-flag: nil
;; End:

; LocalWords:  mpd noidle
