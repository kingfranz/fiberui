(ns fiberui.data
  (:require [fiberui.utils      		:as utils])
  (:require [fiberui.config      		:as config])
  (:require [fiberui.db      		    :as db])
  (:require [clojure.spec               :as s])
  (:require [clojure.math.numeric-tower :as math])
  (:require [clj-time.core              :as t])
  (:require [clj-time.format            :as f])
  (:require [clj-time.local             :as l])
  (:require [taoensso.timbre            :as timbre
             :refer [log  trace  debug  info  warn  error  fatal  report
                     logf tracef debugf infof warnf errorf fatalf reportf spy get-env]])
  (:require [taoensso.timbre.appenders.core :as appenders])
  (:require [clojure.set   				:as set :refer [difference superset?]]))


;;------------------------------------------------------------------------------------

(def ^:private member-example
	{:member-id     3
	 :name          "Sören Svensson"
	 :contact       [{:type :address :value "Lindåsen Höjen 52, 54592 Älgarås"}
					 {:type :email   :value "name1@domain.se"}
					 {:type :email   :value "soren@turbomx5.com" :preferred true}
					 {:type :phone   :value "0703-643025"}
					 {:type :phone   :value "090-773636"}]
	 :debit-credit  [{:date "2016-12-5"
	 				  :amount -500
	 				  :tax 0.0
	 				  :type :membership-fee
	 				  :member-id 3
	 				  :year 2016
	 				  :months #{1 2 3 4 5 6 7 8 9 10 11 12}}]
	 :from-to       {:from "2016-12-5" :to "2016-12-5"}
	 :estates       [{:estate-id "MBEF82" :from-to {:from "2016-12-5" :to "2016-12-5"}}]
	 :note          ""})

;;------------------------------------------------------------------------------------

(defn contains-preferred?
	[contacts]
	;(println "contacts:" contacts)
	(->> contacts (map second) (filter :preferred) not-empty))

(def every-month #{1 2 3 4 5 6 7 8 9 10 11 12})

(defn is-estate-id?
    [x]
    (and (utils/is-string? x) (re-matches #"MBEF[0-9]+" x)))

(s/def :fiber/estate-id      is-estate-id?)
(s/def :fiber/date           utils/date?)
(s/def :fiber/amount         #(and (number? %) (> (math/abs %) 1)))
(s/def :fiber/tax            number?)
(s/def :fiber/from           utils/date?)
(s/def :fiber/to             utils/date?)
(s/def :fiber/from-to        (s/keys :req-un [:fiber/from] :opt-un [:fiber/to]))
(s/def :fiber/note           string?)
(s/def :fiber/year           int?)
(s/def :fiber/months         (s/and set? #(set/superset? every-month %)))
(s/def :fiber/type           #{:membership-fee :connection-fee :operator-fee})
(s/def :fiber/dc-entry       (s/keys :req-un [:fiber/date
											  :fiber/amount
											  :fiber/tax
											  :fiber/type
											  :member/member-id
											  :fiber/year
											  :fiber/months]))
(s/def :addr/value           utils/is-string?)
(s/def :addr/type            #{:address})
(s/def :member/addr-entry    (s/keys :req-un [:addr/type :addr/value] :opt-un [:member/preferred]))
(s/def :email/value          utils/valid-email?)
(s/def :email/type           #{:email})
(s/def :member/email-entry   (s/keys :req-un [:email/type :email/value] :opt-un [:member/preferred]))
(s/def :phone/value    	     utils/valid-phone?)
(s/def :phone/type     	     #{:phone})
(s/def :member/phone-entry 	 (s/keys :req-un [:phone/type :phone/value] :opt-un [:member/preferred]))
(s/def :member/contact-entry (s/or :addr :member/addr-entry
							 	   :email :member/email-entry
							 	   :phone :member/phone-entry))
(s/def :member/estates-entry (s/keys :req-un [:fiber/estate-id :fiber/from-to]))

(s/def :member/name          utils/is-string?)
(s/def :member/contact       (s/and vector?
									(s/+ :member/contact-entry)
									contains-preferred?))
(s/def :member/member-id     utils/is-pos-int?)
(s/def :member/estates       (s/and vector? (s/* :member/estates-entry)))
(s/def :fiber/debit-credit   (s/and vector? (s/* :fiber/dc-entry)))

;;------------------------------------------------------------------------------------

(def member-spec (s/keys :req-un [:member/member-id
								  :member/name
                               	  :member/contact
                               	  :fiber/from-to
                        		  :fiber/debit-credit
                         		  :fiber/note
                               	  :member/estates]))

;;------------------------------------------------------------------------------------

(defn is-member?
	[member]
	(not= (s/conform member-spec member) :clojure.spec/invalid))

(defn validate-member
	[member]
	(if-not (is-member? member)
    	(do
    		(error "------- Invalid member -----------")
    		(error (s/explain-str member-spec member))
    		(error "-------------------------------------")
    		(error member)
    		(throw (Exception. "Invalid member data")))
    	member))

(defn get-all-members
	[]
	(map validate-member (db/get-all-members)))

(defn is-from-to?
	[x]
	(not= (s/conform :fiber/from-to x) :clojure.spec/invalid))

(defn is-debit-credit?
	[x]
	(not= (s/conform :fiber/debit-credit x) :clojure.spec/invalid))

(defn is-contacts?
	[x]
	(not= (s/conform :member/contact x) :clojure.spec/invalid))

(defn mk-email1-str
    [contacts]
    {:pre [(is-contacts? contacts)]}
	(let [email1 (filter #(and (= (:type %) :email) (:preferred %)) contacts)]
        (if (empty? email1)
            ""
            (:value email1))))

(defn preferred-contact
	[member]
	{:pre [(is-member? member)]}
	(if-let [pref (filter :preferred (:contact member))]
		(:value (first pref))
		(throw (Exception. (str (:name member) " har ingen vald kontakt")))))

(defn mk-member-str
	[member]
	{:pre [(is-member? member)]}
	(str "ID: " (:member-id member) " " (:name member) " " (preferred-contact member)))

(defmulti within-from-to (fn [x y] [(if (= (type x) clojure.lang.PersistentArrayMap) :pam) (get {org.joda.time.DateTime :dt
											java.lang.String :str
											java.lang.Long :int} (type y))]))
(defmethod within-from-to [:pam :dt]
	[ft dt]
	{:pre [(is-from-to? ft)]}
	(t/within? (t/interval (f/parse (:from ft)) (f/parse (:to ft))) dt))

(defmethod within-from-to [:pam :str]
	[ft dt]
	{:pre [(is-from-to? ft)]}
	(t/within? (t/interval (f/parse (:from ft)) (f/parse (:to ft))) (f/parse dt)))

(defmethod within-from-to [:pam :int]
	[ft dt]
	{:pre [(is-from-to? ft)]}
	(t/within? (t/interval (f/parse (:from ft)) (f/parse (:to ft))) (t/date-time dt 12 31)))

(defn sum-debit-credit
	[member-id year entry tags]
	{:pre [(int? member-id) (int? year) (is-debit-credit? entry) (seq tags)]}
	(if-let [sum (->> entry
					 :debit-credit
					 (filter #(= (:year %) year))
					 (filter #(= (:member-id %) member-id))
					 (filter #(some #{(:type %)} tags))
					 (map :amount)
					 (reduce +))]
		(float sum)
		0.0))

(defn get-member-name
	[member]
	{:pre [(is-member? member)]}
	(:name member))

;;------------------------------------------------------------------------------------

(def ^:private estate-example
	{:estate-id        "MBEF82"
	 :location         "Myrhult 1:2"
	 :address          "Lindåsen Höjen 52, 54592 Älgarås"
	 :debit-credit     [{:date "2016-12-5"
	 					 :amount 40
	 					 :tax 10
	 					 :type :connection-fee
	 					 :member-id 3
	 					 :year 2016
	 					 :months #{1 2 3}}]
	 :billing-interval [{:year 2016 :months 12} {:year 2017 :months 3}]
	 :activity         [{:year 2016 :months #{1 2 3 10 11 12}}]
	 :from-to          {:from "2016-12-5" :to "2016-12-5"}
	 :note             ""})

;;------------------------------------------------------------------------------------

(s/def :billing/months          int?)
(s/def :estate/activity-entry   (s/keys :req-un [:fiber/year :fiber/months]))
(s/def :estate/billing-entry    (s/keys :req-un [:estate/year :billing/months]))

(s/def :estate/address          utils/is-string?)
(s/def :estate/location         utils/is-string?)
(s/def :estate/billing-interval (s/and vector? (s/+ :estate/billing-entry)))
(s/def :estate/activity         (s/and vector? (s/+ :estate/activity-entry) #(apply distinct? (map :year %))))

;;------------------------------------------------------------------------------------

(def estate-spec (s/keys :req-un [:fiber/estate-id
									:estate/location
									:estate/address
                        			:fiber/note
                        			:fiber/from-to
                           			:fiber/debit-credit
                                    :estate/billing-interval
                                    :estate/activity]))

;;------------------------------------------------------------------------------------

(defn is-estate?
	[x]
	(not= (s/conform estate-spec x) :clojure.spec/invalid))

(defn validate-estate
	[estate]
	{:pre [(is-estate? estate)]}
	(if-not (is-estate? estate)
		(do
			(error "------- Invalid estate -----------")
			(error (s/explain-str estate-spec estate))
			(error "-------------------------------------")
			(error estate)
			(throw (Exception. "Invalid estate data")))
		estate))

(defn get-all-estates
	[]
	(map validate-estate (db/get-all-estates)))

(defn is-yearly-bill
	[estate year]
	{:pre [(is-estate? estate) (int? year)]}
	(->> estate :billing-interval (filter #(= year (:year %))) first :months (= 12)))

(defn mk-estate-str
	[estate]
	{:pre [(is-estate? estate)]}
	(str "ID: " (:estate-id estate) " " (:location estate) " " (:address estate)))

(defn get-activity
	[estate year]
	{:pre [(is-estate? estate) (int? year)]}
	(apply set/union (map :months (filter #(= (:year %) year) (:activity estate)))))

(defn update-estate-billing
	[estate]
	{:pre [(is-estate? estate)]}
	(let [bill-years   (set (map :year (:billing-interval estate)))
		  bill-last    (:months (last (sort-by :year (:billing-interval estate))))
		  dc-years     (set (map :year (:debit-credit estate)))
		  target-years (conj dc-years (utils/year))
		  new-years    (set/difference target-years bill-years)
		  added        (map #(hash-map :year % :months (if (nil? bill-last) 3 bill-last)) new-years)]
		;(println "ueb:" (:billing-interval estate) added)
		;(println "update-estate-billing:" (concat (:billing-interval estate) added))
		;(println "update-estate-billing:" (assoc estate :billing-interval (concat (:billing-interval estate) added)))
		(assoc estate :billing-interval (vec (concat (:billing-interval estate) added)))))

(defn months-billing
	[estate year]
	{:pre [(is-estate? estate) (int? year)]}
	(let [the-year (filter #(= (:year %) year) (:billing-interval estate))]
		(if (empty? the-year)
			(:months (last (sort-by :year (:billing-interval estate))))
			(:months (first the-year)))))

(defn is-owned-by
	[estate member]
	{:pre [(is-estate? estate) (is-member? member)]}
	(some #{(:estate-id estate)} (map :estate-id (:estates member))))

(defn get-estate-activity
	[estate year]
	{:pre [(is-estate? estate) (int? year)]}
	(if-let [s1 (first (filter #(= (:year %) year) (:activity estate)))]
		(:months s1)
		#{}))

(defn get-estates-from-member
	[member year]
	{:pre [(is-member? member) (int? year)]}
	(let [estate-ids (set (map :estate-id (filter #(within-from-to (:from-to %) year) (:estates member))))]
		(filter #(contains? estate-ids (:estate-id %)) (get-all-estates))))


(defn get-estate-address
	[estate]
	{:pre [(is-estate? estate)]}
	(:address estate))

(defn get-estate-id
	[estate]
	{:pre [(is-estate? estate)]}
	(:estate-id estate))

;;------------------------------------------------------------------------------------

(defn is-estate?
	[estate]
	(not= (s/conform estate-spec estate) :clojure.spec/invalid))

;;------------------------------------------------------------------------------------

(def ^:private config-example
	{:entered    "2017-01-20 12:23:34"
	 :membership {:fee 500 :tax 0.0  :start-year 2017 :start-month 1}
	 :connection {:fee 40  :tax 0.25 :start-year 2017 :start-month 1}
	 :operator   {:fee 90  :tax 0.0  :start-year 2017 :start-month 1}})

(s/def :conf/fee         utils/is-pos-int?)
(s/def :conf/tax         (s/and double? #(>= % 0.0) #(< % 1.0)))
(s/def :conf/start-month int?)
(s/def :conf/start-year  int?)
(s/def :conf/fee-entry   (s/keys :req-un [:conf/fee :conf/tax :conf/start-year :conf/start-month]))

(s/def :conf/entered    utils/date-time?)
(s/def :conf/membership :conf/fee-entry)
(s/def :conf/connection :conf/fee-entry)
(s/def :conf/operator   :conf/fee-entry)

(def config-spec (s/keys :req-un [:conf/entered
								  :conf/membership
								  :conf/connection
								  :conf/operator]))

(defn is-config?
	[config]
	(not= (s/conform config-spec config) :clojure.spec/invalid))

(defn validate-config
	[config]
	(if-not (is-config? config)
    	(do
    		(error "------- Invalid config -----------")
    		(error (s/explain-str config-spec config))
    		(error "-------------------------------------")
    		(throw (Exception. "Invalid config data")))
    	config))

;;------------------------------------------------------------------------------------

(defn add-member
    [member]
    (db/add-member (validate-member member)))

(defn add-estate
    [estate]
    (db/add-estate (validate-estate estate)))

(defn add-config
    [config]
    (db/add-config (validate-config config)))

(defn calc-saldo
	[d-c]
	(reduce + (map #(+ (:amount %) (:tax %)) d-c)))

(defn was-owner?
	[estate member year]
	{:pre [(int? year) (is-estate? estate) (is-member? member)]}
	(filter #(and (= (:estate-id %) (:estate-id estate))
					 (within-from-to (:from-to %) year))
			(:estates member)))

(defn is-owner?
	[estate member]
	{:pre [(is-estate? estate) (is-member? member)]}
	(filter #(and (= (:estate-id %) (:estate-id estate))
					 (not (:to (:from-to %))))
		    (:estates member)))

(defn estates-owned-at-year
	[member year]
	{:pre [(int? year) (is-member? member)]}
	(filter #(within-from-to (:from-to %) year) (:estates member)))

(defn get-owners-from-estate
	[estate year]
	{:pre [(int? year) (is-estate? estate)]}
	(->> (get-all-members) (filter #(was-owner? estate % year))))

(defn get-current-owner-from-estate
	[estate]
	{:pre [(is-estate? estate)]}
	(->> (get-all-members) (filter #(is-owner? estate %)) first))

;;------------------------------------------------------------------------------------

(defn get-estate
	[estate-id]
	{:pre [(is-estate-id? estate-id)]}
	(->> (get-all-estates) (filter #(= estate-id (:estate-id %))) first))

(defn get-member
	[member-id]
	{:pre [(int? member-id)]}
	(->> (get-all-members) (filter #(= member-id (:member-id %))) first))

(defn get-members-with-estates
	[year]
	{:pre [(int? year)]}
	(->> (get-all-members) (filter #(seq (estates-owned-at-year % year))) (sort-by :name)))

(defn active-member?
	[member]
	{:pre [(is-member? member)]}
	(nil? (get-in member [:from-to :to])))

;;------------------------------------------------------------------------------------

(defn is-yearly?
	[estate year]
	{:pre [(int? year) (is-estate? estate)]}
	(= (months-billing year estate) 12))

;;------------------------------------------------------------------------------------

(defn membership-charged?
	[member year]
	{:pre [(int? year) (is-member? member)]}
	(seq (filter #(and (utils/this-year? year (:date %))
					   (= (:type %) :membership-fee)
					   (neg? (:amount %)))
				 (:debit-credit member))))

(defn get-members-not-charged-membership
	[year]
	{:pre [(int? year)]}
	(->> (get-all-members)
		 (filter #(not (membership-charged? year %)))
		 (sort-by :name)))

;;------------------------------------------------------------------------------------

(defn yearly-connection-charged?
	[estate year]
	{:pre [(int? year) (is-estate? estate)]}
	(seq (filter #(and (utils/this-year? year (:date %))
					   (= (:type %) :connection-fee)
					   (= every-month (:months %))
					   (neg? (:amount %)))
				 (:debit-credit estate))))

(defn get-estates-not-charged-yearly-con
	[year]
	{:pre [(int? year)]}
	(->> (get-all-estates)
		 (filter #(and (is-yearly? year %) (not (yearly-connection-charged? year %))))))

;;------------------------------------------------------------------------------------

(defn yearly-operator-charged?
	[estate year]
	{:pre [(int? year) (is-estate? estate)]}
	(seq (filter #(and (utils/this-year? year (:date %))
					   (= (:type %) :operator-fee)
					   (= every-month (:months %))
					   (neg? (:amount %)))
				 (:debit-credit estate))))

(defn get-estates-not-charged-yearly-oper
	[year]
	(->> (get-all-estates)
		 (filter #(and (is-yearly? year %) (not (yearly-operator-charged? year %))))))

;;------------------------------------------------------------------------------------

(defn operator-charged?
	[estate months year]
	{:pre [(int? year) (is-estate? estate) (set? months)]}
	(seq (filter #(and (utils/this-year? year (:date %))
					   (= (:type %) :operator-fee)
					   (= (set/intersection months (:months %)) months)
					   (neg? (:amount %)))
				 (:debit-credit estate))))

(defn get-estates-not-charged-oper
	[months year]
	{:pre [(int? year) (set? months)]}
	(->> (get-all-estates)
		 (filter #(and (not (is-yearly? year %))
					   (seq (set/intersection months (get-activity % year)))
					   (not (operator-charged? year months %))))))

;;------------------------------------------------------------------------------------

(defn conn-charged?
	[estate months year]
	{:pre [(int? year) (is-estate? estate) (set? months)]}
	(seq (filter #(and (utils/this-year? year (:date %))
					   (= (:type %) :connection-fee)
					   (= (set/intersection months (:months %)) months)
					   (neg? (:amount %)))
				 (:debit-credit estate))))

(defn get-estates-not-charged-conn
	[months year]
	{:pre [(int? year) (set? months)]}
	(->> (get-all-estates)
		 (filter #(and (not (is-yearly? year %))
					   (not (conn-charged? year months %))))))

;;------------------------------------------------------------------------------------

(defn member-id-exist?
	[member-id]
	{:pre [(int? member-id)]}
	(->> (get-all-members)
		 (filter #(= member-id (:member-id %)))
		 (seq)))

(defn estate-id-exist?
	[estate-id]
	{:pre [(is-estate-id? estate-id)]}
	(->> (get-all-estates)
		 (filter #(= estate-id (:estate-id %)))
		 (seq)))

(defn get-latest-config
	[]
	(->> (db/get-all-configs)
		(sort-by :entered)
		(last)))

(defn get-config
	[months year]
	{:pre [(int? year) (> year config/min-year) (< year config/max-year) (set? months) (seq months)]}
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
		  configs (->> (db/get-all-configs)
		  			   (remove #(> (config-sort %) (mk-idx year (apply max months))))
		  			   (validate-config)
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


