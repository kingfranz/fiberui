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
    (db/persists! on)
    (if on
    	(db/force-persist!)))

(defn add-member
    [member]
    (db/put! :member member))

(defn add-estate
    [house]
    (db/put! :estate house))

(defn add-config
    [conf]
    (db/put! :config conf))

(defn get-latest-config
	[]
	(last (sort-by :entered (db/documents :config))))

(defn get-all-members
	[]
	(db/documents :member))

(defn get-all-estates
	[]
	(db/documents :estate))

(defn member-id-exist?
	[id]
	(not (empty? (db/query :member :where [#(= id (:member-id %))]))))

(defn estate-id-exist?
	[id]
	(not (empty? (db/query :estate :where [#(= id (:estate-id %))]))))

(defn get-owner-from-estate
	[house]
	(first (db/query :member :where [#(some #{(:estate-id house)} (:estates %))])))

(defn get-estates-from-member
	[memb]
	(db/query :estate :where [#(some #{(:estate-id %)} (:estates memb))]))

(defn get-estate
	[id]
	(first (db/query :estate :where [#(= id (:estate-id %))])))

(defn get-members-with-estates
	[]
	(sort-by :name (db/query :member :where [#(not-empty (:estates %))])))

(defn active-member?
	[member]
	(nil? (get-in member [:from-to :to])))

(defn membership-charged?
	[member]
	(seq (filter #(and (utils/this-year? (:date %))
					   (= (:type %) :membership-fee)
					   (neg? (:amount %)))
				 (:debit-credit member))))

(defn get-members-not-charged
	[]
	(sort-by :name (db/query :member :where [#(not (membership-charged? %))])))

