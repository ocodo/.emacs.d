;;; db-pg.el --- A PostgreSQL adapter for emacs-db

;; Copyright (C) 2012  Nic Ferrier
;; Author: Nic Ferrier <nic@ferrier.me.uk>
;; Version: 0.0.3
;; Package-Version: 20130131.1902
;; Keywords: data comm database postgresql
;; Created: 23 December 2012
;; Package-Requires: ((pg "0.12")(db "0.0.6"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is a PostgreSQL adapter for emacs-db. Emacs-DB can be found in
;; the Marmalade repository.

;; See the README for usage.

(require 'pg)
(require 'json)
(require 'db)

(defconst db-pg/table-query
  "select c.relname
from pg_catalog.pg_class c
  left join pg_catalog.pg_namespace n on n.oid = c.relnamespace
where n.nspname = 'public'"
  "The query we use to find the list of tables.

This isn't ideal because it forces the use of 'public' as the
schema name but that could be fixed if we ever need non-public
schemas (we will).")

(defun db-pg/list-tables (con)
  "Get the list of tables from connection CON."
  (let ((res (pg:exec con db-pg/table-query)))
    (loop for row in (pg:result res :tuples)
       collect (car row))))

(defun db-pg/ref->spec (ref)
  "Convert the `db' REF to `pg:connect' details."
  (let ((pg-spec (plist-get ref :pg-spec)))
    (list (plist-get pg-spec :db)
          (plist-get pg-spec :username)
          ;; FIXME - work out how to get defaults into here
          (or (plist-get pg-spec :password) "")
          (or (plist-get pg-spec :host) "localhost")
          (or (plist-get pg-spec :port) 5432))))

(defun db-pg/create-table (con table column)
  "Create a suitable TABLE with COLUMN for db-pg use."
  (let ((result
         (pg:exec
          con
          (format
           "create table %s (%s hstore);"
           table
           column))))
    (pg:result result :status)))

(defun db-pg (reference)
  "Make a Postgresql database utlizing Hstore types.

REFERENCE comes from the call to `db-make' and MUST include:

  `:username' key with a username to connect to the postgresql db
  `:db' key with a postgresql database name
  `:table' key with a table name to use
  `:column' key with a column name to use
  `:key' key with key name to use

And can also include:

  `:host' key with a postgresql server hostname
  `:port' key with the tcp port of the postgresql server

This function checks for the existance of `:table' and if it does
not exist it causes the table to be created with the necessary
HSTORE column by the function `db-pg/create-table'."
  (destructuring-bind (&key
                       db
                       username
                       (host "localhost")
                       (password "")
                       (port 5432)
                       table
                       column
                       key) (cdr reference)
    (let* ((db-spec
            (list
             :get 'db-pg/get
             :put 'db-pg/put
             :map 'db-pg/map
             :pg-spec
             (list :db db :username username
                   :host host :password password :port port
                   :table table :column column :key key))))
      ;; Check that the table exists
      (with-pg-connection
          con (db-pg/ref->spec db-spec)
        (let* ((tables (db-pg/list-tables con))
               (table-exists (member table tables)))
          (unless table-exists
            (db-pg/create-table con table column))))
      ;; Return the database
      db-spec)))


(defun db-pg/alist->hstore (alist)
  "Convert ALIST to a potsgresql Hstore representation.

Hstore representation is like this: key-a=>value,key-b=>value."
  (loop for (key . val) in alist
     if (> (length result) 0)
     concat "," into result
     concat (format "%s=>\"%s\"" key val) into result
     finally return result))

(defun db-pg/select (column table &optional where-key where-val)
  (format
   "select '{' ||
  (select array_to_string(array_agg('\"'
   || item.key || '\":\"'
   || item.value || '\"'), ',') as json from each(%s) item) || '}'
  from %s%s"
   column table
   (if where-key
       (format
        " where %s::hstore -> '%s' = '%s'"
        column where-key
        ;; FIXME - this is ok for ints and such but not strings
        (cond
          ((stringp where-val) (format "%s" where-val))
          (t where-val))) "")))

(defun db-pg/insert (column table values)
  (format
   "insert into %s (%s) values ('%s')"
   table
   column
   (db-pg/alist->hstore values)))

(defun db-pg/update (column table values key key-value)
  (format
   "update %s set %s = '%s' where %s::hstore -> '%s' = '%s'"
   table
   column
   (db-pg/alist->hstore values)
   column
   key
   key-value))

(defun db-pg/json-decode (from-str)
  "Do the JSON reading."
  (let* ((json-key-type 'string))
    (json-read-from-string from-str)))

(defconst db-pg-log-sql t)

(defun db-pg/get (keyval db)
  "Postgresql key based retrieve."
  (let ((db-spec (db-pg/ref->spec db)))
    (with-pg-connection con db-spec
      (let* (collector
             (pg-spec (plist-get db :pg-spec))
             (column (plist-get pg-spec :column))
             (table (plist-get pg-spec :table))
             (key (plist-get pg-spec :key))
             (select-sql
              (db-pg/select column table key keyval)))
        (when db-pg-log-sql
          (with-current-buffer (get-buffer-create
                                ;; Fixme - want dbname in here as well
                                (format "*db-pg-%s-%s*" table column))
            (save-excursion
              (goto-char (point-max))
              (insert select-sql "\n"))))
        (pg:for-each
         con select-sql
         (lambda (result)
           (setq
            collector
            (append
             collector
             (list (db-pg/json-decode (caar result)))))))
        ;; We only want the first one
        (car collector)))))

(defun db-pg/put (key value db)
  (let ((db-spec (db-pg/ref->spec db)))
    (with-pg-connection con db-spec
      ;; FIXME - what if the key is not in the value?
      (let* ((pg-spec (plist-get db :pg-spec))
             (column (plist-get pg-spec :column))
             (table (plist-get pg-spec :table))
             (key-name (plist-get pg-spec :key))
             (select-sql (db-pg/select column table key-name key))
             (result (pg:exec con select-sql))
             (row (pg:result result :tuples)))
        (if row
            (pg:exec
             con
             (db-pg/update column table value key-name key))
            ;; Else insert
            (pg:exec
             con (db-pg/insert column table value)))
        ;; Now something to return
        (db-pg/get key db)))))

(defun db-pg/map (func db &optional query filter)
  "Call FUNC for every value in DB or just those matching QUERY.

FILTER causes it "
  (let ((db-spec (db-pg/ref->spec db)))
    (with-pg-connection con db-spec
      ;; FIXME - what if the key is not in the value?
      (let* ((pg-spec (plist-get db :pg-spec))
             (column (plist-get pg-spec :column))
             (table (plist-get pg-spec :table))
             (key (plist-get pg-spec :key))
             (select-sql (db-pg/select column table))
             (result (pg:exec con select-sql)))
        (loop for (row . rest) in (pg:result result :tuples)
           append
             (let ((alist (db-pg/json-decode row)))
               (funcall func (aget alist key) alist)))))))


;; Ensure the pg stuff is included in the db config
(puthash 'db-pg 'db-pg db/types)

(provide 'db-pg)

;;; db-pg.el ends here
