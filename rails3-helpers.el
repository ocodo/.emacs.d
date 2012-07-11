;; Rails 3 helpers...

(defun rails3/create-migration (migration-name)
   "Create a Rails 3 migration buffer."
   (interactive "sName (e.g. model_add_column ) : ")
   (let*
       (  
        ( timestamp  (format-time-string "%Y%m%d%H%M%S")  )
        ( filename  (format "%s_%s.rb"  timestamp migration-name) )
        ( classname (camelize migration-name) )
       )
     (create-file-buffer filename)
     (switch-to-buffer filename)
     (insert (format "class %s < ActiveRecord::Migration\n" classname)) 
     (insert "  def change\n")
     (insert "  end\n")
     (insert "end\n")))


;; ------------------------------------------------
;; add_column :table, :column, :type

;; add_index 

;; change_column :table, 

;; change_table :products do |t|
;;  t.remove :description, :name
;;  t.string :part_number
;;  t.index :part_number
;;  t.rename :upccode, :upc_code
;; end

;; create_table :table do |t|
;;    t.type :name
;;    t.timestamps
;; end

;; drop_table :table

;; remove_column :table, :column

;; remove_index 

;; rename_column
;; ---------------------------------------------------
;; Data types
;; :binary
;; :boolean
;; :date
;; :datetime
;; :decimal
;; :float
;; :integer
;; :primary_key
;; :string
;; :text
;; :time
;; :timestamp


(defun camelize (s)
  "Convert under_score string S to CamelCase string."
  (mapconcat 'identity (mapcar
                        '(lambda (word) (capitalize (downcase word)))
                        (split-string s "_")) ""))

