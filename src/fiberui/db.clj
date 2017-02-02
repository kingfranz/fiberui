(ns fiberui.db
	(:require [fiberui.utils   :as utils])
	(:require [gardendb.core   :as db]))

(defn db-init
    []
    (do
        (db/initialize! :db-name "fiber-db" :clear? false :persists? true)
        (db/load!)))

(defn set-persist
    [on]
    (db/persists? on)
    (if on
    	(db/force-persist!)))

(defn add-member
    [member]
    (db/put! :member member))

(defn add-estate
    [estate]
    (db/put! :estate estate))

(defn add-config
    [config]
    (db/put! :config config))

(defn get-all-members
	[]
	(db/documents :member))

(defn get-all-estates
	[]
	(db/documents :estate))

(defn get-all-configs
	[]
	(db/documents :config))

;;------------------------------------------------------------------------------------

