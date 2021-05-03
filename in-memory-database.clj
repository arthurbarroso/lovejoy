(ns in-memory-database.core)

(def memory-db (atom {}))
(defn read-db [] @memory-db)
(defn write-db [new-db]
  (reset! memory-db new-db))

(defn create-table [table-name]
  (let [new-db (into (read-db) {table-name {:data [] :indexes {}}})]
    (write-db new-db)))
(create-table :chicletes)
(read-db)
(create-table :sup)
(defn drop-table [table-name]
  (let [new-db (dissoc (read-db) table-name)]
    (write-db new-db)))
(drop-table :sup)

(defn insert [table-name record id-key]
  (let [db (read-db)
        new-db (update-in db [table-name :data] conj record)
        index (- (count (get-in new-db [table-name :data])) 1)]
    (write-db
     (update-in new-db [table-name :indexes id-key] assoc (id-key record) index))))

(read-db)
(insert :chicletes {:name "Bubble gum" :stock 30} :name)

(defn select-* [table-name]
  (get-in (read-db) [table-name :data]))
(select-* :chicletes)
(defn select-*-where [table-name field field-value]
  (let [db (read-db)
        index (get-in db [table-name :indexes field field-value])
        data (get-in db [table-name :data])]
    (get data index)))

(select-*-where :chicletes :name "Bubble gum")

(defn new-insert [table-name record id-key]
  (if (select-*-where table-name id-key (id-key record))
    (println "Record with " id-key ": " (id-key record) "is already in the database. Not going to proceed")
    (let [db (read-db)
          new-db (update-in db [table-name :data] conj record)
          index (- (count (get-in new-db [table-name :data])) 1)]
      (write-db
       (update-in new-db [table-name :indexes id-key] assoc (id-key record) index)))))

(new-insert :chicletes {:name "Chicletíssimo" :stock 3} :name)
(new-insert :chicletes {:name "Chicletíssimo" :stock 9} :name)
