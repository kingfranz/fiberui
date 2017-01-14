(ns fiberui.db
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