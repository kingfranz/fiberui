(ns fiberui.db
	(:require [fiberui.utils   :as utils])
	(:require [gardendb.core   :as db]))

(defn db-init
    []
    (do
        (db/initialize! :db-name "fiber-db" :clear? false :persists? true)
        (db/load!)))

(defn add-member
    [member]
    (db/put! :member member))

(defn add-estate
    [house]
    (db/put! :estate house))

(defn get-all-members
	[]
	(db/documents :member))

(defn member-id-exist?
	[id]
	(let [id-num (utils/parse-int id)
		  result (db/query :member :where [#(= id-num (:member-id %))])]
		;(println "id:" id-num "result:" result)
		(not (empty? result))))

