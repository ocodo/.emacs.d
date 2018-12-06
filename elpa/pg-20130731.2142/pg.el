;;; pg.el --- Emacs Lisp interface to the PostgreSQL RDBMS
;;;
;;; Author: Eric Marsden <emarsden@laas.fr>
;;; Maintainer: Helmut Eller <heller@common-lisp.net>
;;; Version: 0.13+ (sorta)
;; Package-Version: 20130731.2142
;;; Keywords: data comm database postgresql
;;;
;;; Copyright: (C) 1999-2005  Eric Marsden
;;; Copyright: (C) 2005-2006  Eric Marsden, Helmut Eller
;;   
;;     This program is free software; you can redistribute it and/or
;;     modify it under the terms of the GNU General Public License as
;;     published by the Free Software Foundation; either version 2 of
;;     the License, or (at your option) any later version.
;;     
;;     This program is distributed in the hope that it will be useful,
;;     but WITHOUT ANY WARRANTY; without even the implied warranty of
;;     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;     GNU General Public License for more details.
;;     
;;     You should have received a copy of the GNU General Public
;;     License along with this program; if not, write to the Free
;;     Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;;     MA 02111-1307, USA.
;;


;;; Commentary:

;;; Overview ==========================================================
;;
;; This module lets you access the PostgreSQL object-relational DBMS
;; from Emacs, using its socket-level frontend/backend protocol. The
;; module is capable of automatic type coercions from a range of SQL
;; types to the equivalent Emacs Lisp type. This is a low level API,
;; and won't be useful to end users. Should work with GNU Emacs 19.34
;; and up, and XEmacs 20 and up. Performance is very poor when not
;; byte-compiled.

;;; Entry points =======================================================
;;
;; (with-pg-connection con (dbname user [password host port]) &body body)
;;     A macro which opens a connection to database DBNAME, executes the
;;     BODY forms then disconnects. See function `pg:connect' for details
;;     of the connection arguments.
;;
;; (with-pg-transaction con &body body)
;;     A macro which executes the BODY forms wrapped in an SQL transaction.
;;     CON is a connection to the database. If an error occurs during the
;;     execution of the forms, a ROLLBACK instruction is executed.
;;
;; (pg:connect dbname user [password host port]) -> connection
;;     Connect to the database DBNAME on HOST (defaults to localhost)
;;     at PORT (defaults to 5432) via TCP/IP and log in as USER. If
;;     the database requires a password, send PASSWORD as clear text.
;;     Set the output date type to 'ISO', and initialize our type
;;     parser tables.
;;
;; (pg:exec connection &rest sql) -> pgresult
;;     Concatenate the SQL strings and send to the backend. Retrieve
;;     all the information returned by the database and return it in
;;     an opaque record PGRESULT.
;;
;; (pg:result pgresult what &rest args) -> info
;;     Extract information from the PGRESULT. The WHAT keyword can be
;;     one of
;;          * :connection
;;          * :status
;;          * :attributes
;;          * :tuples
;;          * :tuple tupleNumber
;;          * :oid
;;     `:connection' allows you to retrieve the database connection.
;;     `:status' is a string returned by the backend to indicate the
;;     status of the command; it is something like "SELECT" for a
;;     select command, "DELETE 1" if the deletion affected a single
;;     row, etc. `:attributes' is a list of tuples providing metadata:
;;     the first component of each tuple is the attribute's name as a
;;     string, the second an integer representing its PostgreSQL type,
;;     and the third an integer representing the size of that type.
;;     `:tuples' returns all the data retrieved from the database, as a
;;     list of lists, each list corresponding to one row of data
;;     returned by the backend. `:tuple num' can be used to extract a
;;     specific tuple (numbering starts at 0). `:oid' allows you to
;;     retrieve the OID returned by the backend if the command was an
;;     insertion; the OID is a unique identifier for that row in the
;;     database (this is PostgreSQL-specific, please refer to the
;;     documentation for more details).
;;
;; (pg:disconnect connection) -> nil
;;     Close the database connection.
;;
;; (pg:for-each connection select-form callback)
;;     Calls CALLBACK on each tuple returned by SELECT-FORM. Declares
;;     a cursor for SELECT-FORM, then fetches tuples using repeated
;;     executions of FETCH 1, until no results are left. The cursor is
;;     then closed. The work is performed within a transaction. When
;;     you have a large amount of data to handle, this usage is more
;;     efficient than fetching all the tuples in one go.
;;
;;     If you wish to browse the results, each one in a separate
;;     buffer, you could have the callback insert each tuple into a
;;     buffer created with (generate-new-buffer "myprefix"), then use
;;     ibuffer's "/ n" to list/visit/delete all buffers whose names
;;     match myprefix.
;;
;; (pg:databases connection) -> list of strings
;;     Return a list of the databases available at this site (a
;;     database is a set of tables; in a virgin PostgreSQL
;;     installation there is a single database named "template1").
;;
;; (pg:tables connection) -> list of strings
;;     Return a list of the tables present in the database to which we
;;     are currently connected. Only include user tables: system
;;     tables are excluded.
;;
;; (pg:columns connection table) -> list of strings
;;     Return a list of the columns (or attributes) in TABLE, which
;;     must be a table in the database to which we are currently
;;     connected. We only include the column names; if you want more
;;     detailed information (attribute types, for example), it can be
;;     obtained from `pg:result' on a SELECT statement for that table.
;;
;; (pg:lo-create conn . args) -> oid
;;     Create a new large object (BLOB, or binary large object in
;;     other DBMSes parlance) in the database to which we are
;;     connected via CONN. Returns an OID (which is represented as an
;;     elisp integer) which will allow you to use the large object.
;;     Optional ARGS are a Unix-style mode string which determines the
;;     permissions of the newly created large object, one of "r" for
;;     read-only permission, "w" for write-only, "rw" for read+write.
;;     Default is "r".
;;
;;     Large-object functions MUST be used within a transaction (see
;;     the macro `with-pg-transaction').
;;
;; (pg:lo-open conn oid . args) -> fd
;;     Open a large object whose unique identifier is OID (an elisp
;;     integer) in the database to which we are connected via CONN.
;;     Optional ARGS is a Unix-style mode string as for pg:lo-create;
;;     which defaults to "r" read-only permissions. Returns a file
;;     descriptor (an elisp integer) which can be used in other
;;     large-object functions.
;;
;; (pg:lo-close conn fd)
;;     Close the file descriptor FD which was associated with a large
;;     object. Note that this does not delete the large object; use
;;     `pg:lo-unlink' for that.
;;
;; (pg:lo-read conn fd bytes) -> string
;;     Read BYTES from the file descriptor FD which is associated with
;;     a large object. Return an elisp string which should be BYTES
;;     characters long.
;;
;; (pg:lo-write connection fd buf)
;;     Write the bytes contained in the elisp string BUF to the
;;     large object associated with the file descriptor FD. 
;;
;; (pg:lo-lseek conn fd offset whence)
;;     Do the equivalent of a lseek(2) on the file descriptor FD which
;;     is associated with a large object; ie reposition the read/write
;;     file offset for that large object to OFFSET (an elisp
;;     integer). WHENCE has the same significance as in lseek(); it
;;     should be one of SEEK_SET (set the offset to the absolute
;;     position), SEEK_CUR (set the offset relative to the current
;;     offset) or SEEK_END (set the offset relative to the end of the
;;     file). WHENCE should be an elisp integer whose values can be
;;     obtained from the header file <unistd.h> (probably 0, 1 and 2
;;     respectively).
;;
;; (pg:lo-tell conn oid) -> integer
;;     Do the equivalent of an ftell(3) on the file associated with
;;     the large object whose unique identifier is OID. Returns the
;;     current position of the file offset for the object's associated
;;     file descriptor, as an elisp integer.
;;
;; (pg:lo-unlink conn oid)
;;     Remove the large object whose unique identifier is OID from the
;;     system (in the current implementation of large objects in
;;     PostgreSQL, each large object is associated with an object in
;;     the filesystem).
;;
;; (pg:lo-import conn filename) -> oid
;;     Create a new large object and initialize it to the data
;;     contained in the file whose name is FILENAME. Returns an OID
;;     (as an elisp integer). Note that is operation is only syntactic
;;     sugar around the basic large-object operations listed above.
;;
;; (pg:lo-export conn oid filename)
;;     Create a new file named FILENAME and fill it with the contents
;;     of the large object whose unique identifier is OID. This
;;     operation is also syntactic sugar.
;;
;;
;; Boolean variable `pg:disable-type-coercion' which can be set to
;; non-nil (before initiating a connection) to disable the library's
;; type coercion facility. Default is t.
;;
;;
;; The interface is pretty slow (byte compiling helps a lot). Maybe
;; someone can suggest a better way of reading input from the network
;; stream. Please note that your postmaster has to be started with the
;; `-i' option in order to accept TCP/IP connections (this is not the
;; default). For more information about PostgreSQL see
;; <URL:http://www.PostgreSQL.org/>.
;;
;; Thanks to Eric Ludlam <zappo@gnu.org> for discovering a bug in the
;; date parsing routines, to Hartmut Pilch and Yoshio Katayama for
;; adding multibyte support, and to Doug McNaught and Pavel Janik for
;; bug fixes.


;; SECURITY NOTE: setting up PostgreSQL to accept TCP/IP connections
;; has security implications; please consult the documentation for
;; details. pg.el supports neither the crypt authentication method,
;; nor Kerberos (support for these can't be added to Emacs due to
;; silly US crypto export regulations). However, it is possible to use
;; the port forwarding capabilities of ssh to establish a connection
;; to the backend over TCP/IP, which provides both a secure
;; authentication mechanism and encryption (and optionally
;; compression) of data passing through the tunnel. Here's how to do
;; it (thanks to Gene Selkov, Jr. <selkovjr@mcs.anl.gov> for the
;; description):
;;
;; 1. Establish a tunnel to the backend machine, like this:
;; 
;; 	ssh -L 3333:backend.dom:5432 postgres@backend.dom
;; 
;;    The first number in the -L argument, 3333, is the port number of
;;    your end of the tunnel. The second number, 5432, is the remote
;;    end of the tunnel -- the port number your backend is using. The
;;    name or the address in between the port numbers belongs to the
;;    server machine, as does the last argument to ssh that also includes
;;    the optional user name. Without the user name, ssh will try the
;;    name you are currently logged on as on the client machine. You can
;;    use any user name the server machine will accept, not necessarily
;;    those related to postgres.
;; 
;; 2. Now that you have a running ssh session, you can point pg.el to
;;    the local host at the port number which you specified in step 1.
;;    For example,
;;
;;         (pg:connect "dbname" "user" "password" "localhost" 3333)
;;
;;    You can omit the port argument if you chose 5432 as the local
;;    end of the tunnel, since pg.el defaults to this value.


;;; INSTALL =========================================================
;;
;; Place this file in a directory somewhere in the load-path, then
;; byte-compile it (do a `B' on it in dired, for example). Place a
;; line such as `(require 'pg)' in your emacs initialization file.


;;; TODO ============================================================
;;
;; * add a mechanism for parsing user-defined types. The user should
;;   be able to define a parse function and a type-name; we query
;;   pg_type to get the type's OID and add the information to
;;   pg:parsers.
;;
;; * in a future release I will probably modify the numeric conversion
;;   routines to return elisp floating point values instead of elisp
;;   integers, in order to work around possible overflow problems.


;;; Code:

(eval-and-compile
  (require 'cl))

(defvar pg:disable-type-coercion nil  
  "*Non-nil disables the type coercion mechanism.
The default is nil, which means that data recovered from the database
is coerced to the corresponding Emacs Lisp type before being returned;
for example numeric data is transformed to Emacs Lisp numbers, and
booleans to booleans.

The coercion mechanism requires an initialization query to the
database, in order to build a table mapping type names to OIDs. This
option is provided mainly in case you wish to avoid the overhead of
this initial query. The overhead is only incurred once per Emacs
session (not per connection to the backend).")

;;(defvar pg:coding-system nil
;;  "*The coding system that PostgreSQL was compiled to use. Should be
;;nil if PostgreSQL wasn't compiled with multibyte support, or for
;;example the symbol `utf-8' if your PostgreSQL was compiled with
;;`--enable-multibyte=UNICODE' and you are using a MULE-UCS-enabled
;;Emacs.")

(defconst pg:NAMEDATALEN 32)              ; postgres_ext.h
(defconst pg:PG_PROTOCOL_LATEST_MAJOR 2)  ; libpq/pgcomm.h
(defconst pg:PG_PROTOCOL_63_MAJOR     1)
(defconst pg:PG_PROTOCOL_LATEST_MINOR 0)
(defconst pg:SM_DATABASE 64)
(defconst pg:SM_USER     32)
(defconst pg:SM_OPTIONS  64)
(defconst pg:SM_UNUSED   64)
(defconst pg:SM_TTY      64)

(defconst pg:AUTH_REQ_OK       0)
(defconst pg:AUTH_REQ_KRB4     1)
(defconst pg:AUTH_REQ_KRB5     2)
(defconst pg:AUTH_REQ_PASSWORD 3)
(defconst pg:AUTH_REQ_CRYPT    4)
(defconst pg:AUTH_REQ_MD5      5)

(defconst pg:STARTUP_MSG            7)
(defconst pg:STARTUP_KRB4_MSG      10)
(defconst pg:STARTUP_KRB5_MSG      11)
(defconst pg:STARTUP_PASSWORD_MSG  14)

(defconst pg:StartupPacketSize
  (+ 4 4 pg:SM_DATABASE pg:SM_USER pg:SM_OPTIONS pg:SM_UNUSED pg:SM_TTY))

(defconst pg:MAX_MESSAGE_LEN    8192)   ; libpq-fe.h

(defconst pg:INV_ARCHIVE 65536)         ; fe-lobj.c
(defconst pg:INV_WRITE   131072)
(defconst pg:INV_READ    262144)
(defconst pg:LO_BUFIZE   1024)

;; this regular expression works in Emacs 21 and XEmacs, but not Emacs
;; 20.x (no match-exactly-n-times facility)
;; (defconst pg:ISODATE_REGEX (concat
;; "\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\) " ; Y-M-D
;; "\\([0-9]\\{2\\}\\):\\([0-9]\\{2\\}\\):\\([.0-9]+\\)" ; H:M:S.S
;; "\\([-+][0-9]+\\)")) ; TZ

(defconst pg:ISODATE_REGEX 
  (concat "\\([0-9]+\\)-\\([0-9][0-9]\\)-\\([0-9][0-9]\\) " ; Y-M-D
	  "\\([0-9][0-9]\\):\\([0-9][0-9]\\):\\([.0-9]+\\)" ; H:M:S.S
	  "\\([-+][0-9]+\\)?")) ; TZ

;; alist of (oid . parser) pairs. This is built dynamically at
;; initialization of the connection with the database (once generated,
;; the information is shared between connections).
(defvar pg:parsers '())

(defstruct pgcon process pid secret (binaryp nil) encoding)
(defstruct pgresult connection status attributes tuples portal)

(defun pg:flush (connection)
  ;;(accept-process-output (pgcon-process connection))
  )

;; this is ugly because lambda lists don't do destructuring
(defmacro with-pg-connection (con open-args &rest body)
  "Bindspec is of the form (connection open-args), where OPEN-ARGS are
as for PG:CONNECT. The database connection is bound to the variable
CONNECTION. If the connection is unsuccessful, the forms are not
evaluated. Otherwise, the BODY forms are executed, and upon
termination, normal or otherwise, the database connection is closed."
  (declare
   (debug (sexp sexp &rest form))
   (indent 2))
  (let ((open-argsv (make-symbol "open-argsv")))
    `(let* ((,open-argsv ,open-args)
            (,con (apply 'pg:connect ,open-argsv)))
       (unwind-protect
            (progn ,@body)
         (when ,con (pg:disconnect ,con))))))

(defmacro with-pg-transaction (con &rest body)
  "Execute BODY forms in a BEGIN..END block.
If a PostgreSQL error occurs during execution of the forms, execute
a ROLLBACK command.
Large-object manipulations _must_ occur within a transaction, since
the large object descriptors are only valid within the context of a
transaction."
  (declare
   (debug (sexp &rest form))
   (indent 1))
  (let ((exc-sym (gensym)))
    `(progn
       (pg:exec ,con "BEGIN WORK")
       (condition-case ,exc-sym
           (prog1 (progn ,@body)
             (pg:exec ,con "COMMIT WORK"))
         (error
          (message "PostgreSQL error %s" ,exc-sym)
          (pg:exec ,con "ROLLBACK WORK"))))))

(defun pg:for-each (conn select-form callback)
  "Create a cursor for SELECT-FORM, and call CALLBACK for each result.
Uses the PostgreSQL database connection CONN. SELECT-FORM must be an
SQL SELECT statement. The cursor is created using an SQL DECLARE
CURSOR command, then results are fetched successively until no results
are left. The cursor is then closed.

The work is performed within a transaction. The work can be
interrupted before all tuples have been handled by THROWing to a tag
called 'pg-finished."
  (let ((cursor (symbol-name (gensym "pgelcursor"))))
    (catch 'pg-finished
      (with-pg-transaction conn
         (pg:exec conn "DECLARE " cursor " CURSOR FOR " select-form)
         (unwind-protect 
             (loop for res = (pg:result (pg:exec conn "FETCH 1 FROM " cursor) :tuples)
                   until (zerop (length res))
                   do (funcall callback res))
           (pg:exec conn "CLOSE " cursor))))))


(defun* pg:connect (dbname user
                   &optional (password "") (host "localhost") (port 5432)
		   (encoding 'latin-1))
  "Initiate a connection with the PostgreSQL backend.
Connect to the database DBNAME with the username USER, on PORT of
HOST, providing PASSWORD if necessary. Return a connection to the
database (as an opaque type). PORT defaults to 5432, HOST to
\"localhost\", and PASSWORD to an empty string."  
  (let* ((buf (generate-new-buffer " *PostgreSQL*"))
         process connection
         (user-packet-length (+ pg:SM_USER pg:SM_OPTIONS pg:SM_UNUSED pg:SM_TTY)))
    (with-current-buffer buf
      (set-buffer-multibyte nil))
    ;;(message "open-network...")
    (setq process (open-network-stream "postgres" buf host port))
    ;;(message "open-network... done.")
    (set-process-coding-system process 'binary 'binary)
    (setq connection (make-pgcon :process process :encoding encoding))
    ;; send the startup packet
    (pg:send-int connection pg:StartupPacketSize 4)
    (pg:send-int connection pg:PG_PROTOCOL_63_MAJOR 2)
    (pg:send-int connection pg:PG_PROTOCOL_LATEST_MINOR 2)
    (pg:send connection dbname pg:SM_DATABASE)
    (pg:send connection user user-packet-length)
    (pg:flush connection)
    (loop for c = (pg:read-char connection) do
      (cond ((eq ?E c) 
	     (error "Backend error: %s" (pg:read-string connection 4096)))
            ((eq ?R c)
             (let ((areq (pg:read-net-int connection 4)))
               (cond
                ((= areq pg:AUTH_REQ_OK)
                 (and (not pg:disable-type-coercion)
                     (null pg:parsers)
                     (pg:initialize-parsers connection))
		 (let ((enc (ecase encoding
			      (latin-1 "LATIN-1")
			      (utf-8 "UTF-8"))))
		   (pg:exec connection 
			    (format "SET client_encoding = '%s';" enc)))
                 (pg:exec connection "SET datestyle = 'ISO';")
                 (return-from pg:connect connection))
                ((= areq pg:AUTH_REQ_PASSWORD)
                 (pg:send-int connection (+ 5 (length password)) 4)
                 (pg:send connection password)
                 (pg:send-int connection 0 1)
                 (pg:flush connection))
                ((= areq pg:AUTH_REQ_CRYPT)
                 (error "Crypt authentication not supported"))
                ((= areq pg:AUTH_REQ_KRB4)
                 (error "Kerberos4 authentication not supported"))
                ((= areq pg:AUTH_REQ_KRB5)
                 (error "Kerberos5 authentication not supported"))
                ((= areq pg:AUTH_REQ_MD5)
		 (let* ((salt (pg:read-chars connection 4))
			(crypted (pg:md5-encode user password salt)))
		   ;;(message "md5 %S %S %S => %S\n"
		   ;;	    user password salt crypted)
		   (pg:send-int connection (+ 5 (length crypted)) 4)
		   (pg:send connection crypted)
		   (pg:send-int connection 0 1)
		   (pg:flush connection)))
		(t
                 (error "Can't do that type of authentication: %s" areq)))))
            (t
             (error "Problem connecting: expected an authentication response"))))))

(defun* pg:exec (connection &rest args)
  "Execute the SQL command given by the concatenation of ARGS
on the database to which we are connected via CONNECTION. Return
a result structure which can be decoded using `pg:result'."
  (let ((sql (apply #'concat args))
        (tuples '())
        (attributes '())
        (result (make-pgresult :connection connection)))
    (if (> (length sql) pg:MAX_MESSAGE_LEN)
        (error "SQL statement too long: %s" sql))
    (let ((str (encode-coding-string (format "%c%s%c" ?Q sql 0)
				     (pgcon-encoding connection))))
      ;;;(debug nil str)
      (pg:send connection str))
    (pg:flush connection)
    (loop for c = (pg:read-char connection) do
          (case c
            ;; AsynchronousNotify
            (?A
             (let ((pid (pg:read-int connection 4))
                   (msg (pg:read-string connection pg:MAX_MESSAGE_LEN)))
               (message "Asynchronous notify %s" msg)))

            ;; BinaryRow
            (?B
             (setf (pgcon-binaryp connection) t)
             (or attributes (error "Tuple received before metadata"))
             (push (pg:read-tuple connection attributes) tuples))

            ;; CompletedResponse
            (?C
             (let* ((status (pg:read-string connection pg:MAX_MESSAGE_LEN)))
               (setf (pgresult-status result) status)
               (setf (pgresult-tuples result) (nreverse tuples))
               (setf (pgresult-attributes result) attributes)
               (return-from pg:exec result)))

            ;; TextDataTransfer
            (?D
             (setf (pgcon-binaryp connection) nil)
             (or attributes (error "Tuple received before metadata"))
             (push (pg:read-tuple connection attributes) tuples))

            ;; ErrorResponse
            (?E
             (let ((msg (pg:read-string connection pg:MAX_MESSAGE_LEN)))
               (error "Backend error: %s" msg)))

            ;; EmptyQueryResponse
            (?I
             (let ((c (pg:read-char connection)))
               ))

            ;; BackendKeyData
            (?K
             (setf (pgcon-pid connection) (pg:read-net-int connection 4))
             (setf (pgcon-secret connection) (pg:read-net-int connection 4)))
             
            
            ;; NoticeResponse
            (?N
             (let ((notice (pg:read-string connection pg:MAX_MESSAGE_LEN)))
               (message "NOTICE: %s" notice)
	       ;;(debug nil notice)
	       ))

            ;; CursorResponse
            (?P
             (let ((portal (pg:read-string connection pg:MAX_MESSAGE_LEN)))
               (setf (pgresult-portal result) portal)))

            ;; RowDescription
            (?T
             (and attributes (error "Cannot handle multiple result group"))
             (setq attributes (pg:read-attributes connection)))

            ;; ReadyForQuery
            (?Z t)
             
            (t (error "Unknown response type from backend: %s" c))))))

(defun pg:result (result what &rest arg)
  "Extract WHAT component of RESULT.
RESULT should be a structure obtained from a call to `pg:exec',
and the keyword WHAT should be one of
   :connection -> return the connection object
   :status -> return the status string provided by the database
   :attributes -> return the metadata, as a list of lists
   :tuples -> return the data, as a list of lists
   :tuple n -> return the nth component of the data
   :oid -> return the OID (a unique identifier generated by PostgreSQL
           for each row resulting from an insertion)"
  (cond ((eq :connection what) (pgresult-connection result))
        ((eq :status what)     (pgresult-status result))
        ((eq :attributes what) (pgresult-attributes result))
        ((eq :tuples what)     (pgresult-tuples result))
        ((eq :tuple what)
         (let ((which (if (integerp (car arg)) (car arg)
                        (error "%s is not an integer" arg)))
               (tuples (pgresult-tuples result)))
           (nth which tuples)))
        ((eq :oid what)
         (let ((status (pgresult-status result)))
           (if (string= "INSERT" (substring status 0 6))
               (string-to-number (substring status 7 (position ? status :start 7)))
               (error "Only INSERT commands generate an oid: %s" status))))
        (t
         (error "Unknown result request %s" what))))

(defun pg:disconnect (connection)
  "Close the database connection.
This command should be used when you have finished with the database.
It will release memory used to buffer the data transfered between
PostgreSQL and Emacs. CONNECTION should no longer be used."
  (pg:send connection "X")
  (pg:flush connection)
  (delete-process (pgcon-process connection))
  (kill-buffer (process-buffer (pgcon-process connection))))


;; type coercion support ==============================================
;;
;; When returning data from a SELECT statement, PostgreSQL starts by
;; sending some metadata describing the attributes. This information
;; is read by `pg:read-attributes', and consists of each attribute's
;; name (as a string), its size (in bytes), and its type (as an oid
;; which identifies a row in the PostgreSQL system table pg_type). Each
;; row in pg_type includes the type's name (as a string).
;;
;; We are able to parse a certain number of the PostgreSQL types (for
;; example, numeric data is converted to a numeric Emacs Lisp type,
;; dates are converted to the Emacs date representation, booleans to
;; Emacs Lisp booleans). However, there isn't a fixed mapping from a
;; type to its OID which is guaranteed to be stable across database
;; installations, so we need to build a table mapping OIDs to parser
;; functions.
;;
;; This is done by the procedure `pg:initialize-parsers', which is run
;; the first time a connection is initiated with the database from
;; this invocation of Emacs, and which issues a SELECT statement to
;; extract the required information from pg_type. This initialization
;; imposes a slight overhead on the first request, which you can avoid
;; by setting `pg:disable-type-coercion' to non-nil if it bothers you.
;; ====================================================================


;; this is a var not a const to allow user-defined types (a PostgreSQL
;; feature not present in ANSI SQL). The user can add a (type-name .
;; type-parser) pair and call `pg:initialize-parsers', after which the
;; user-defined type should be returned parsed from `pg:result'.
;; Untested.
(defvar pg:type-parsers
  `(("bool"      . ,'pg:bool-parser)
    ("char"      . ,'pg:text-parser)
    ("char2"     . ,'pg:text-parser)
    ("char4"     . ,'pg:text-parser)
    ("char8"     . ,'pg:text-parser)
    ("char16"    . ,'pg:text-parser)
    ("text"      . ,'pg:text-parser)
    ("varchar"   . ,'pg:text-parser)
    ("numeric"   . ,'pg:number-parser)
    ("int2"      . ,'pg:number-parser)
    ("int28"     . ,'pg:number-parser)
    ("int4"      . ,'pg:number-parser)
    ("oid"       . ,'pg:number-parser)
    ("float4"    . ,'pg:number-parser)
    ("float8"    . ,'pg:number-parser)
    ("money"     . ,'pg:number-parser)
    ("abstime"   . ,'pg:isodate-parser)
    ("date"      . ,'pg:date-parser)
    ("timestamp" . ,'pg:isodate-parser)
    ("datetime"  . ,'pg:isodate-parser)
    ("time"      . ,'pg:text-parser)     ; preparsed "15:32:45"
    ("reltime"   . ,'pg:text-parser)     ; don't know how to parse these
    ("timespan"  . ,'pg:text-parser)
    ("tinterval" . ,'pg:text-parser)))

;; see `man pgbuiltin' for details on PostgreSQL builtin types
(defun pg:number-parser (str) (string-to-number str))

;; bound in pg:read-tuple
(defvar pg::text-encoding nil)
(defsubst pg:text-parser (str)
  (assert pg::text-encoding)
  (decode-coding-string str pg::text-encoding))

(defun pg:bool-parser (str)
  (cond ((string= "t" str) t)
        ((string= "f" str) nil)
        (t (error "Badly formed boolean from backend: %s" str))))

;; format for ISO dates is "1999-10-24"
(defun pg:date-parser (str)
  (let ((year  (string-to-number (substring str 0 4)))
        (month (string-to-number (substring str 5 7)))
        (day   (string-to-number (substring str 8 10))))
    (encode-time 0 0 0 day month year)))

;;  format for abstime/timestamp etc with ISO output syntax is
;;;    "1999-01-02 14:32:53+01"
;; which we convert to the internal Emacs date/time representation
;; (there may be a fractional seconds quantity as well, which the regex
;; handles)
(defun pg:isodate-parser (str)
  (if (string-match pg:ISODATE_REGEX str)  ; is non-null
      (let ((year    (string-to-number (match-string 1 str)))
	    (month   (string-to-number (match-string 2 str)))
	    (day     (string-to-number (match-string 3 str)))
	    (hours   (string-to-number (match-string 4 str)))
	    (minutes (string-to-number (match-string 5 str)))
	    (seconds (round (string-to-number (match-string 6 str))))
	    (tzs (when (match-string 7 str)
		   (* 3600 (string-to-number (match-string 7 str))))))
	(encode-time seconds minutes hours day month year tzs))
    (error "Badly formed ISO timestamp from backend: %s" str)))


(defun pg:initialize-parsers (connection)
  (let* ((pgtypes (pg:exec connection "SELECT typname,oid FROM pg_type"))
         (tuples (pg:result pgtypes :tuples)))
    (setq pg:parsers '())
    (mapcar
     #'(lambda (tuple)
       (let* ((typname (first tuple))
              (oid (string-to-number (second tuple)))
              (type (assoc* typname pg:type-parsers :test #'string=)))
         (if (consp type)
             (push (cons oid (cdr type)) pg:parsers))))
     tuples)))

(defun pg:parse (str oid)
  (let ((parser (assoc* oid pg:parsers :test #'eq)))
    (if (consp parser)
        (funcall (cdr parser) str)
      str)))


;; md5 auth

(defun pg:md5-encode (user password salt)
  (format "md5%s" (pg:md5-key-salt (pg:md5-key-salt password user) salt)))

(defun pg:md5-key-salt (key salt)
  (let ((d (pg:md5-hex-digest (concat key salt))))
    (assert (= (length d) 32))
    d))

(defun pg:md5-hex-digest (string)
  (cond ((fboundp 'md5) (md5 string))
	(t
	 (let ((tmpfile (make-temp-name "/tmp/md5-hex")))
	   (with-temp-file tmpfile (insert string))
	   (unwind-protect
	       (with-temp-buffer
		 (let ((c (call-process "md5sum" tmpfile (current-buffer))))
		   (assert (zerop c))
		   (goto-char (point-min))
		   (search-forward " ")
		   (buffer-substring 1 (1- (point)))))
	     (delete-file tmpfile))))))

;; large object support ================================================
;;
;; Humphrey: Who is Large and to what does he object?
;;
;; Large objects are the PostgreSQL way of doing what most databases
;; call BLOBs (binary large objects). In addition to being able to
;; stream data to and from large objects, PostgreSQL's
;; object-relational capabilities allow the user to provide functions
;; which act on the objects.
;;
;; For example, the user can define a new type called "circle", and
;; define a C or Tcl function called `circumference' which will act on
;; circles. There is also an inheritance mechanism in PostgreSQL. 
;;
;;======================================================================
(defvar pg:lo-initialized nil)
(defvar pg:lo-functions '())

(defun pg:lo-init (connection)
  (let* ((res (pg:exec connection
                       "SELECT proname, oid from pg_proc WHERE "
                       "proname = 'lo_open' OR "
                       "proname = 'lo_close' OR "
                       "proname = 'lo_creat' OR "
                       "proname = 'lo_unlink' OR "
                       "proname = 'lo_lseek' OR "
                       "proname = 'lo_tell' OR "
                       "proname = 'loread' OR "
                       "proname = 'lowrite'")))
    (setq pg:lo-functions '())
    (mapc
     #'(lambda (tuple)
         (push (cons (car tuple) (cadr tuple)) pg:lo-functions))
     (pg:result res :tuples))
    (setq pg:lo-initialized t)))

;; fn is either an integer, in which case it is the OID of an element
;; in the pg_proc table, and otherwise it is a string which we look up
;; in the alist `pg:lo-functions' to find the corresponding OID.
(defun pg:fn (connection fn integer-result &rest args)
  (or pg:lo-initialized (pg:lo-init connection))
  (let ((fnid (cond ((integerp fn) fn)
                    ((not (stringp fn))
                     (error "Expecting a string or an integer"))
                    ((assoc fn pg:lo-functions) ; blech
                     (cdr (assoc fn pg:lo-functions)))
                    (t
                     (error "Unknown builtin function: %S" fn)))))
    (pg:send-char connection ?F)
    (pg:send-char connection 0)
    (pg:send-int connection fnid 4)
    (pg:send-int connection (length args) 4)
    (mapc #'(lambda (arg)
              (cond ((integerp arg)
                     (pg:send-int connection 4 4)
                     (pg:send-int connection arg 4))
                    ((stringp arg)
                     (pg:send-int connection (length arg) 4)
                     (pg:send connection arg))
                    (t
                     (error "Unknown fastpath type: %S" arg))))
          args)
    (pg:flush connection)
    (loop with result = '()
          for c = (pg:read-char connection) do
          (case c
             ;; ErrorResponse
            (?E (error (pg:read-string connection 4096)))

            ;; FunctionResultResponse
            (?V (setq result t))
            
            ;; Nonempty response
            (?G
             (let* ((len (pg:read-net-int connection 4))
                    (res (if integer-result
                             (pg:read-net-int connection len)
                           (pg:read-chars connection len))))
               (setq result res)))

            ;; NoticeResponse
            (?N
             (let ((notice (pg:read-string connection pg:MAX_MESSAGE_LEN)))
               (message "NOTICE: %s" notice))
             (unix-sync))
            
            ;; ReadyForQuery
            (?Z t)

            ;; end of FunctionResult
            (?0 (return result))
            
            (t (error "Unexpected character in pg:fn: ?%c" c))))))

;; returns an OID
(defun pg:lo-create (connection &optional args)
  (let* ((modestr (or args "r"))
         (mode (cond ((integerp modestr) modestr)
		     ((string= "r" modestr) pg:INV_READ)
                     ((string= "w" modestr) pg:INV_WRITE)
                     ((string= "rw" modestr)
                      (logior pg:INV_READ pg:INV_WRITE))
                     (t (error "pg:lo-create: bad mode %s" modestr))))
         (oid (pg:fn connection "lo_creat" t mode)))
    (cond ((not (integerp oid))
           (error "Didn't return an OID: %S" oid))
          ((zerop oid)
           (error "Can't create large object"))
          (t oid))))

;; args = modestring (default "r", or "w" or "rw")
;; returns a file descriptor for use in later pg:lo-* procedures        
(defun pg:lo-open (connection oid &optional args)
  (let* ((modestr (or args "r"))
         (mode (cond ((integerp modestr) modestr)
		     ((string= "r" modestr) pg:INV_READ)
                     ((string= "w" modestr) pg:INV_WRITE)
                     ((string= "rw" modestr)
                      (logior pg:INV_READ pg:INV_WRITE))
                     (t (error "pg:lo-open: bad mode %s" modestr))))
         (fd (pg:fn connection "lo_open" t oid mode)))
    (unless (integerp fd)
      (error "Couldn't open large object"))
    fd))

(defsubst pg:lo-close (connection fd)
  (pg:fn connection "lo_close" t fd))

(defsubst pg:lo-read (connection fd bytes)
  (pg:fn connection "loread" nil fd bytes))

(defsubst pg:lo-write (connection fd buf)
  (pg:fn connection "lowrite" t fd buf))
  
(defsubst pg:lo-lseek (connection fd offset whence)
  (pg:fn connection "lo_lseek" t fd offset whence))

(defsubst pg:lo-tell (connection oid)
  (pg:fn connection "lo_tell" t oid))
  
(defsubst pg:lo-unlink (connection oid)
  (pg:fn connection "lo_unlink" t oid))

;; returns an OID
;; FIXME should use unwind-protect here
(defun pg:lo-import (connection filename)
  (let* ((buf (get-buffer-create (format " *pg-%s" filename)))
         (oid (pg:lo-create connection "rw"))
         (fdout (pg:lo-open connection oid "w"))
         (pos (point-min)))
    (save-excursion
      (set-buffer buf)
      (insert-file-contents-literally filename)
      (while (< pos (point-max))
        (pg:lo-write
         connection fdout
         (buffer-substring-no-properties pos (min (point-max) (incf pos 1024)))))
      (pg:lo-close connection fdout)
      (kill-buffer buf)
      oid)))

(defun pg:lo-export (connection oid filename)
  (let* ((buf (get-buffer-create (format " *pg-%d" oid)))
         (fdin (pg:lo-open connection oid "r")))
    (save-excursion
      (set-buffer buf)
      (do ((str (pg:lo-read connection fdin 1024)
                (pg:lo-read connection fdin 1024)))
          ((or (not str)
               (zerop (length str))))
        (insert str))
      (pg:lo-close connection fdin)
      (write-file filename)
      (kill-buffer buf))))



;; DBMS metainformation ================================================
;;
;; Metainformation such as the list of databases present in the
;; database management system, list of tables, attributes per table.
;; This information is not available directly, but can be deduced by
;; querying the system tables.
;;
;; Based on the queries issued by psql in response to user commands
;; `\d' and `\d tablename'; see file
;; /usr/local/src/pgsql/src/bin/psql/psql.c
;; =====================================================================
(defun pg:databases (conn)
  "Return a list of the databases available at this site."
  (let ((res (pg:exec conn "SELECT datname FROM pg_database")))
    (apply #'append (pg:result res :tuples))))

(defun pg:tables (conn)
  "Return a list of the tables present in this database."
  (let ((res (pg:exec conn "SELECT relname FROM pg_class, pg_user WHERE "
                      "(relkind = 'r' OR relkind = 'i' OR relkind = 'S') AND "
                      "relname !~ '^pg_' AND usesysid = relowner ORDER BY relname")))
    (apply #'append (pg:result res :tuples))))
    
(defun pg:columns (conn table)
  "Return a list of the columns present in TABLE."
  (let* ((sql (format "SELECT * FROM %s WHERE 0 = 1" table))
         (res (pg:exec conn sql)))
    (mapcar #'car (pg:result res :attributes))))

(defun pg:backend-version (conn)
  "Version an operating environment of the backend as a string."
  (let ((res (pg:exec conn "SELECT version()")))
    (first (pg:result res :tuple 0))))


;; support routines ============================================================

;; Attribute information is as follows
;;    attribute-name (string)
;;    attribute-type as an oid from table pg_type
;;    attribute-size (in bytes?)
(defun pg:read-attributes (connection)
  (let ((attribute-count (pg:read-net-int connection 2))
        (attributes '()))
    (do ((i attribute-count (- i 1)))
        ((zerop i) (nreverse attributes))
      (let ((type-name (pg:read-string connection pg:MAX_MESSAGE_LEN))
            (type-id   (pg:read-net-int connection 4))
            (type-len  (pg:read-net-int connection 2)))
        (push (list type-name type-id type-len) attributes)))))

;; a bitmap is a string, which we interpret as a sequence of bytes
(defun pg:bitmap-ref (bitmap ref)
;;   (multiple-value-bind (char-ref bit-ref)
;;       (floor* ref 8)
    (let ((int (aref bitmap (floor ref 8))))
      (logand 128 (ash int (mod ref 8)))))

;; the backend starts by sending a bitmap indicating which tuples are
;; NULL
(defun pg:read-tuple (connection attributes)
  (let* ((num-attributes (length attributes))
         ;; (num-bytes (car (ceiling* num-attributes 8)))
         (num-bytes (ceiling num-attributes 8))
         (bitmap (pg:read-chars connection num-bytes))
         (correction (if (pgcon-binaryp connection) 0 -4))
         (tuples '()))
    (do ((i 0 (+ i 1))
         (type-ids (mapcar #'second attributes) (cdr type-ids)))
        ((= i num-attributes) (nreverse tuples))
      (cond ((zerop (pg:bitmap-ref bitmap i))
             (push nil tuples))
            (t
             (let* ((len (+ (pg:read-net-int connection 4) correction))
                    (raw (pg:read-chars connection (max 0 len)))
		    (pg::text-encoding (pgcon-encoding connection))
                    (parsed (pg:parse raw (car type-ids))))
               (push parsed tuples)))))))

(defun pg:read-char (connection)
  ;;(message "read-char: %d %d" (point) (buffer-size))
  (let ((process (pgcon-process connection)))
    (with-current-buffer (process-buffer process)
      (unless (char-after 1)
        (pg::accept-process-output process 0.001)
	(while (not (char-after 1))
	  (pg::accept-process-output process 0.1)))
      (prog1 (char-after 1)
	;;(message "read-char: %d %d => %c" 
	;;	 (point) (buffer-size) (char-after 1))
	(delete-region 1 2)))))

;; FIXME should be more careful here; the integer could overflow.
(defun pg:read-net-int (connection bytes)
  (do ((i bytes (- i 1))
       (accum 0))
      ((zerop i) accum)
    (setq accum (+ (* 256 accum) (pg:read-char connection)))))

(defun pg:read-int (connection bytes)
  (do ((i bytes (- i 1))
       (multiplier 1 (* multiplier 256))
       (accum 0))
      ((zerop i) accum)
    (incf accum (* multiplier (pg:read-char connection)))))

(defun pg:read-chars (connection howmany)
  (let* ((process (pgcon-process connection)))
    (with-current-buffer (process-buffer process)
      (when (< (buffer-size) howmany)
        (pg::accept-process-output process 0.002)
	(while (< (buffer-size) howmany)
	  (pg::accept-process-output process 0.2)))
      (prog1 (buffer-substring-no-properties 1 (1+ howmany))
	(delete-region 1 (1+ howmany))))))

(defvar pg::accept-process-output-supports-floats 
  (ignore-errors (accept-process-output nil 0.0) t))

(defvar pg::inside-accept-process-output nil)

(defun pg::accept-process-output (&optional process timeout)
  "Like `accept-process-output' but the TIMEOUT argument can be a float."
  (when pg::inside-accept-process-output
    (error "pg::accept-process-output called recursively"))
  (let ((pg::inside-accept-process-output t))
    (cond (pg::accept-process-output-supports-floats
	   (accept-process-output process timeout nil 1))
	  (t
	   (accept-process-output 
	    process 
	    (if timeout (truncate timeout))
	    ;; Emacs21 uses microsecs; Emacs22 millisecs
	    (if timeout (truncate (* timeout 1000000))))))))

(defun pg::process-send (process string)
  "Wrapper aroud process-send-string."
  (assert (not pg::inside-accept-process-output))
  (process-send-string process string))
  
;; read a null-terminated string
(defun pg:read-string (connection maxbytes)
  (loop for i from 1 to maxbytes
        for ch = (pg:read-char connection)
        until (= ch ?\0)
        concat (char-to-string ch)))

;; higher order bits first
(defun pg:send-int (connection num bytes)
  (let ((process (pgcon-process connection))
        (str (make-string bytes 0))
        (i (- bytes 1)))
    (while (>= i 0)
      (aset str i (% num 256))
      (setq num (floor num 256))
      (decf i))
    (pg::process-send process str)))
  
(defun pg:send-char (connection char)
  (let ((process (pgcon-process connection)))
    (pg::process-send process (char-to-string char))))

(defun pg:send (connection str &optional bytes)
  (let ((process (pgcon-process connection))
	(data (if (and (numberp bytes) (> bytes (length str)))
		  (concat str (make-string (- bytes (length str)) 0))
		str)))
    (pg::process-send process data)))

;; This (limited) testing code assumes you have a database user
;; "postgres" with no password accessible from the localhost, and
;; a database named "template1". It should clean up after itself.
;;
;; * is the postmaster running?
;; * was the postmaster started with the `-i' commandline option?
;;
;; This code has been tested with GNU Emacs 19.34, 20.3 and 20.6, and
;; XEmacs 20.4, on Solaris and linuxppc. It should work with
;; PostgreSQL 6.x, 7.0, 7.1.2. 

;; (defmacro with-pgtest-connection (&rest body)
;;   `(with-pg-connection conn ("template1" "postgres")
;;       ,@body))

;; (defun pg:test ()
;;   (with-pgtest-connection
;;    (message "Running pg.el tests against backend %s"
;;             (pg:backend-version conn))
;;    (let ((databases (pg:databases conn)))
;;      (if (member "pgeltest" databases)
;;          (pg:exec conn "DROP DATABASE pgeltest"))
;;      (pg:exec conn "CREATE DATABASE pgeltest"))
;;    (message "Testing insertions...")
;;    (pg:test-insert)
;;    (message "Testing date routines...")
;;    (pg:test-date)
;;    (message "Testing field extraction routines...")
;;    (pg:test-result)
;;    (message "Testing large-object routines...")   
;;    (pg:test-lo-read)
;;    (pg:test-lo-import)
;;    (pg:exec conn "DROP DATABASE pgeltest")
;;    (message "Tests passed ok")))
;; 
;; ;; this will be *real* slow unless byte-compiled
;; (defun pg:test-insert ()
;;   (with-pgtest-connection
;;    (let (res)
;;      (pg:exec conn "CREATE TABLE count_test(key int, val int)")
;;      (loop for i from 1 to 100
;;            for sql = (format "INSERT INTO count_test VALUES(%s, %s)"
;;                              i (* i i))
;;            do (pg:exec conn sql))
;;      (setq res (pg:exec conn "SELECT count(val) FROM count_test"))
;;      (assert (= 100 (first (pg:result res :tuple 0))))
;;      (setq res (pg:exec conn "SELECT sum(key) FROM count_test"))
;;      (assert (= 5050 (first (pg:result res :tuple 0))))
;;      (pg:exec conn "DROP TABLE count_test"))))
;; 
;; ;; Testing for the time handling routines. Expected output is
;; ;; something like (in buffer *Messages*)
;; ;;
;; ;; timestamp = (14189 17420)
;; ;; abstime = (14189 17420)
;; ;; time = 19:42:06
;; (defun pg:test-date ()
;;   (with-pgtest-connection
;;    (let (res)
;;      (pg:exec conn "CREATE TABLE date_test(a timestamp, b abstime, c time)")
;;      (pg:exec conn "INSERT INTO date_test VALUES "
;;               "(current_timestamp, 'now', 'now')")
;;      (setq res (pg:exec conn "SELECT * FROM date_test"))
;;      (setq res (pg:result res :tuple 0))
;;      (message "timestamp = %s" (first res))
;;      (message "abstime = %s" (second res))
;;      (message "time = %s" (third res)))
;;    (pg:exec conn "DROP TABLE date_test")))
;;   
;; ;; Testing for the data access functions. Expected output is something
;; ;; like
;; ;;
;; ;; ==============================================
;; ;; status of CREATE is CREATE
;; ;; status of INSERT is INSERT 22506 1
;; ;; oid of INSERT is 22506
;; ;; status of SELECT is SELECT
;; ;; attributes of SELECT are ((a 23 4) (b 1043 65535))
;; ;; tuples of SELECT are ((3 zae) (66 poiu))
;; ;; second tuple of SELECT is (66 poiu)
;; ;; status of DROP is DROP
;; ;; ==============================================
;; (defun pg:test-result ()
;;   (with-pgtest-connection
;;    (let ((r1 (pg:exec conn "CREATE TABLE resulttest (a int, b VARCHAR(4))"))
;;          (r2 (pg:exec conn "INSERT INTO resulttest VALUES (3, 'zae')"))
;;          (r3 (pg:exec conn "INSERT INTO resulttest VALUES (66, 'poiu')"))
;;          (r4 (pg:exec conn "SELECT * FROM resulttest"))
;;          (r5 (pg:exec conn "DROP TABLE resulttest")))
;;      (message "==============================================")
;;      (message "status of CREATE is %s" (pg:result r1 :status))
;;      (message "status of INSERT is %s" (pg:result r2 :status))
;;      (message "oid of INSERT is %s"    (pg:result r2 :oid))
;;      (message "status of SELECT is %s" (pg:result r4 :status))
;;      (message "attributes of SELECT are %s" (pg:result r4 :attributes))
;;      (message "tuples of SELECT are %s" (pg:result r4 :tuples))
;;      (message "second tuple of SELECT is %s" (pg:result r4 :tuple 1))
;;      (message "status of DROP is %s" (pg:result r5 :status))
;;      (message "=============================================="))))
;; 
;; ;; test of large-object interface. Note the use of with-pg-transaction
;; ;; to wrap the requests in a BEGIN..END transaction which is necessary
;; ;; when working with large objects. 
;; (defun pg:test-lo-read ()
;;   (with-pgtest-connection
;;    (with-pg-transaction conn
;;     (let* ((oid (pg:lo-create conn "rw"))
;;            (fd (pg:lo-open conn oid "rw")))
;;       (message "==================================================")
;;       (pg:lo-write conn fd "Hi there mate")
;;       (pg:lo-lseek conn fd 3 0)           ; SEEK_SET = 0
;;       (unless (= 3 (pg:lo-tell conn fd))
;;         (error "lo-tell test failed!"))
;;       (message "Read %s from lo" (pg:lo-read conn fd 7))
;;       (message "==================================================")
;;       (pg:lo-close conn fd)
;;       (pg:lo-unlink conn oid)))))
;;   
;; (defun pg:test-lo-import ()
;;   (with-pgtest-connection
;;    (with-pg-transaction conn
;;     (let ((oid (pg:lo-import conn "/etc/group")))
;;       (pg:lo-export conn oid "/tmp/group")
;;       (cond ((zerop (call-process "diff" nil nil nil "/tmp/group" "/etc/group"))
;;              (message "lo-import test succeeded")
;;              (delete-file "/tmp/group"))
;;             (t
;;              (message "lo-import test failed: check differences")
;;              (message "between files /etc/group and /tmp/group")))
;;       (pg:lo-unlink conn oid)))))
;; 
;; (defun pg:cleanup ()
;;   (interactive)
;;   (loop for b in (buffer-list) do
;;         (if (string-match " \\*PostgreSQL\\*" (buffer-name b))
;;             (kill-buffer b))))


(provide 'pg)

;;; pg.el ends here
