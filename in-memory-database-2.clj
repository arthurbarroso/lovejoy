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

(defn auto-increment-insert [table-name data-to-insert]
  (let [last-item (last (get-in (read-db) [table-name :data]))
        name-already-in-use (filter #(= (:name %) (:name data-to-insert)) (get-in (read-db) [table-name :data]))
        data-to-insert (assoc data-to-insert :id (inc (if (nil? last-item) 0 (:id last-item))))]
    (if (empty? name-already-in-use)
      (write-db (update-in (read-db) [table-name :data] conj data-to-insert))
      "Name already in use! :( Please try a different name")))

(defn get-by-id [table-name id-key]
  (let [db (read-db)
        requested-data (filter #(= (:id %) id-key) (get-in db [table-name :data]))]
    requested-data))

(filter #(= (:name %) "xi3") (get-in (read-db) [:orders :data]))
(read-db)

(auto-increment-insert :orders {:name "ci3"})
(auto-increment-insert :orders {:name "xi33"})

(read-db)
(delete-by-id :orders 1)
(delete-by-id :orders 2)
(delete-by-id :orders 3)

(get-by-id :orders 2)

(filter #(= (:id %) 3) [{:id 2} {:id 3}])
