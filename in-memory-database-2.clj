(ns in-memory-database-2.core)

(def memory-db (atom {}))
(defn read-db [] @memory-db)
(defn write-db [new-db]
  (reset! memory-db new-db))

(defn create-table [table-name]
  (write-db (assoc (read-db) table-name {:data []})))
(create-table :orders)
(read-db)
(create-table :clients)

(defn drop-table [table-name]
  (write-db (dissoc (read-db) table-name)))
(drop-table :clients)

(defn insert [table-name data-to-insert]
  (write-db (update-in (read-db) [table-name :data] conj data-to-insert)))
(insert :orders {:id 1 :product "chiclete" :price 10})
(insert :orders {:id 2 :product "tenis" :price 90})
(insert :orders {:id 3 :product "clojure" :price 900})

(read-db)

(:data (:orders (read-db)))

(remove #(= (:id %) 1) [{:id 1} {:id 2}])

(remove #(= (:id %) 3) (:data (:orders (read-db))))

(defn delete-by-id [table-name id-key]
  (let [db (read-db)
        new-table-data (remove #(= (:id %) id-key) (:data (table-name (read-db))))]
    (write-db (assoc-in db [table-name :data] new-table-data))))
(delete-by-id :orders 3)
