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

(defn add-config
    [conf]
    (db/put! :config conf))

(defn get-all-members
	[]
	(db/documents :member))

(defn get-all-estates
	[]
	(db/documents :estate))

(defn member-id-exist?
	[id]
	(not (empty? (db/query :member :where [#(= id (:member-id %))]))))

