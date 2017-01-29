(ns fiberui.db
	(:require [fiberui.utils   :as utils])
	(:require [fiberui.data    :as data])
	(:require [clojure.set     :as set])
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
    (db/put! :member (data/validate-member member)))

(defn add-estate
    [house]
    (db/put! :estate (data/validate-estate (data/update-estate-billing house))))

(defn add-config
    [conf]
    (db/put! :config (data/validate-config conf)))

(defn get-latest-config
	[]
	(data/validate-config (last (sort-by :entered (db/documents :config)))))

(defn get-config
	[year months]
	{:pre [(int? year) (> year 2010) (< year 2021) (set? months) (seq months)]}
	(let [mk-idx (fn [y m] (+ y (/ m 13)))
		  config-sort (fn [cfg] (mk-idx (:start-year cfg) (:start-month cfg)))
		  update-month (fn [m c] (if (<= (config-sort c) (mk-idx year (key m)))
		  							 {(key m) {:membership {:fee (->> c :membership :fee)
		  							 					    :tax (->> c :membership :tax)
		  							 					    :tot (* (->> c :membership :fee)
		  							 					   		    (->> c :membership :tax (+ 1.0)))}
		  							 		   :connection {:fee (->> c :connection :fee)
		  							 					    :tax (->> c :connection :tax)
		  							 					    :tot (* (->> c :connection :fee)
		  							 					    	    (->> c :connection :tax (+ 1.0)))}
		  							 		   :operator   {:fee (->> c :operator :fee)
		  							 					    :tax (->> c :operator :tax)
		  							 					    :tot (* (->> c :operator :fee)
		  							 					    	    (->> c :operator :tax (+ 1.0)))}}}
		  							 m))
		  configs (->> (db/documents :config)
		  			   (remove #(> (config-sort %) (mk-idx year (apply max months))))
		  			   (data/validate-config)
		  			   (sort-by config-sort))
		  aaa (map #(hash-map % {:membership {:fee 0 :tax 0.0 :tot 0.0}
		  					     :connection {:fee 0 :tax 0.0 :tot 0.0}
		  					     :operator   {:fee 0 :tax 0.0 :tot 0.0}}) months)
		  mmm (loop [m   aaa
			         cfg configs]
					(if (nil? cfg)
						m
						(recur (map #(update-month % (first cfg)) m) (rest cfg))))]
		(into {} (map #(hash-map (:month %) (dissoc % :month)) mmm))))

(defn get-all-members
	[]
	(map data/validate-member (db/documents :member)))

(defn get-all-estates
	[]
	(map #(data/validate-estate %) (db/documents :estate)))

(defn member-id-exist?
	[id]
	(not (empty? (db/query :member :where [#(= id (:member-id %))]))))

(defn estate-id-exist?
	[id]
	(not (empty? (db/query :estate :where [#(= id (:estate-id %))]))))

;;------------------------------------------------------------------------------------

(defn was-owner?
	[estate member year]
	(filter #(and (= (:estate-id %) (:estate-id estate))
					 (data/within-from-to (:from-to %) year)) (:estates member)))

(defn get-owners-from-estate
	[year estate]
	(map data/validate-member (db/query :member :where [#(was-owner? estate % year)])))

;;------------------------------------------------------------------------------------

(defn get-estates-from-member
	[memb]
	(map data/validate-estate (db/query :estate :where [#(some #{(:estate-id %)} (map :estate-id (:estates memb)))])))

(defn get-estate
	[id]
	(data/validate-estate (first (db/query :estate :where [#(= id (:estate-id %))]))))

(defn get-member
	[id]
	(data/validate-member (first (db/query :member :where [#(= id (:member-id %))]))))

(defn get-members-with-estates
	[]
	(map data/validate-member (sort-by :name (db/query :member :where [#(not-empty (:estates %))]))))

(defn active-member?
	[member]
	(nil? (get-in member [:from-to :to])))

;;------------------------------------------------------------------------------------

(defn is-yearly?
	[year estate]
	(= (data/months-billing year estate) 12))

;;------------------------------------------------------------------------------------

(defn membership-charged?
	[year member]
	(seq (filter #(and (utils/this-year? year (:date %))
					   (= (:type %) :membership-fee)
					   (neg? (:amount %)))
				 (:debit-credit member))))

(defn get-members-not-charged-membership
	[year]
	(->> [#(not (membership-charged? year %))]
		 (db/query :member :where)
		 (sort-by :name)
		 (map data/validate-member)))

;;------------------------------------------------------------------------------------

(defn yearly-connection-charged?
	[year estate]
	(seq (filter #(and (utils/this-year? year (:date %))
					   (= (:type %) :connection-fee)
					   (= data/every-month (:months %))
					   (neg? (:amount %)))
				 (:debit-credit estate))))

(defn get-estates-not-charged-yearly-con
	[year]
	(->> [#(and (is-yearly? year %) (not (yearly-connection-charged? year %)))]
		 (db/query :estate :where)
		 (map data/validate-estate)))

;;------------------------------------------------------------------------------------

(defn yearly-operator-charged?
	[year estate]
	(seq (filter #(and (utils/this-year? year (:date %))
					   (= (:type %) :operator-fee)
					   (= data/every-month (:months %))
					   (neg? (:amount %)))
				 (:debit-credit estate))))

(defn get-estates-not-charged-yearly-oper
	[year]
	(->> [#(and (is-yearly? year %) (not (yearly-operator-charged? year %)))]
		 (db/query :estate :where)
		 (map data/validate-estate)))

;;------------------------------------------------------------------------------------

(defn operator-charged?
	[year months estate]
	(seq (filter #(and (utils/this-year? year (:date %))
					   (= (:type %) :operator-fee)
					   (= (set/intersection months (:months %)) months)
					   (neg? (:amount %)))
				 (:debit-credit estate))))

(defn get-estates-not-charged-oper
	[year months]
	(->> [#(and (not (is-yearly? year %))
				(seq (set/intersection months (data/get-activity % year)))
				(not (operator-charged? year months %)))]
		 (db/query :estate :where)
		 (map data/validate-estate)))

;;------------------------------------------------------------------------------------

(defn conn-charged?
	[year months estate]
	(seq (filter #(and (utils/this-year? year (:date %))
					   (= (:type %) :connection-fee)
					   (= (set/intersection months (:months %)) months)
					   (neg? (:amount %)))
				 (:debit-credit estate))))

(defn get-estates-not-charged-conn
	[year months]
	(->> [#(and (not (is-yearly? year %))
				(not (conn-charged? year months %)))]
		 (db/query :estate :where)
		 (map data/validate-estate)))

;;------------------------------------------------------------------------------------
