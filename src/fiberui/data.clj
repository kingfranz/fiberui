(ns fiberui.data
  (:require [fiberui.utils      :as utils      :refer :all])
  (:require [clojure.spec               :as s])
  (:require [clojure.math.numeric-tower :as math])
  (:require [clj-time.core              :as t])
  (:require [clj-time.format            :as f])
  (:require [clj-time.local             :as l])
  (:require [taoensso.timbre            :as timbre
             :refer [log  trace  debug  info  warn  error  fatal  report
                     logf tracef debugf infof warnf errorf fatalf reportf spy get-env]])
  (:require [taoensso.timbre.appenders.core :as appenders])
  (:require [clojure.set   :as set :refer [difference superset?]]))


;;------------------------------------------------------------------------------------

(defn mk-email1-str
    [contact]
    (let [email1 (filter #(and (= (:type %) :email) (:preferred %)) contact)]
        (if (empty? email1)
            ""
            (:value email1))))

(defn preferred-contact
	[member]
	(if-let [pref (filter :preferred (:contact member))]
		(:value (first pref))
		(throw (Exception. (str (:name member) " har ingen vald kontakt")))))

(defn mk-member-str
	[member]
	(str "ID: " (:member-id member) " " (:name member) " " (preferred-contact member)))

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
; [[:addr {:value aaaaaaaa, :type :address, :preferred true}] [:addr {:value bbbbbbbbb, :type :address}]]

(defn contains-preferred?
	[contacts]
	;(println "contains:" contacts)
	(->> contacts (map second) (filter :preferred) not-empty))

(def every-month #{1 2 3 4 5 6 7 8 9 10 11 12})

(s/def :fiber/estate-id      is-estate-id?)
(s/def :fiber/date           date?)
(s/def :fiber/amount         #(and (number? %) (> (math/abs %) 1)))
(s/def :fiber/tax            number?)
(s/def :fiber/from           date?)
(s/def :fiber/to             date?)
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
(s/def :addr/value           is-string?)
(s/def :addr/type            #{:address})
(s/def :member/addr-entry    (s/keys :req-un [:addr/type :addr/value] :opt-un [:member/preferred]))
(s/def :email/value          valid-email?)
(s/def :email/type           #{:email})
(s/def :member/email-entry   (s/keys :req-un [:email/type :email/value] :opt-un [:member/preferred]))
(s/def :phone/value    	     valid-phone?)
(s/def :phone/type     	     #{:phone})
(s/def :member/phone-entry 	 (s/keys :req-un [:phone/type :phone/value] :opt-un [:member/preferred]))
(s/def :member/contact-entry (s/or :addr :member/addr-entry
							 	   :email :member/email-entry
							 	   :phone :member/phone-entry))
(s/def :member/estates-entry (s/keys :req-un [:fiber/estate-id :fiber/from-to]))

(s/def :member/name          is-string?)
(s/def :member/contact       (s/and vector?
									(s/+ :member/contact-entry)
									contains-preferred?))
(s/def :member/member-id     is-pos-int?)
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

(defn validate-member
	[member]
	(if (= (s/conform member-spec member) :clojure.spec/invalid)
    	(do
    		(error "------- Invalid member -----------")
    		(error (s/explain-str member-spec member))
    		(error "-------------------------------------")
    		(error member)
    		(throw (Exception. "Invalid member data")))
    	member))

(defmulti within-from-to (fn [x y] [x y]))
(defmethod within-from-to [map org.joda.time.DateTime]
	[ft dt]
	(t/within? (t/interval (f/parse (:from ft)) (f/parse (:to ft))) dt))

(defmethod within-from-to [map java.lang.String]
	[ft dt]
	(t/within? (t/interval (f/parse (:from ft)) (f/parse (:to ft))) (f/parse dt)))

(defmethod within-from-to [map int]
	[ft dt]
	(t/within? (t/interval (f/parse (:from ft)) (f/parse (:to ft))) (t/date-time dt 6 30)))

(defn sum-debit-credit
	[member-id year entry tags]
	(if-let [sum (->> entry
					 :debit-credit
					 (filter #(= (:year %) year))
					 (filter #(= (:member-id %) member-id))
					 (filter #(some #{(:type %)} tags))
					 (map :amount)
					 (reduce +))]
		(float sum)
		0.0))

(defn is-yearly-bill
	[year estate]
	(->> estate :billing-interval (filter #(= year (:year %))) first :months (= 12)))


;;------------------------------------------------------------------------------------

(defn mk-estate-str
	[estate]
	(str "ID: " (:estate-id estate) " " (:location estate) " " (:address estate)))

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

(s/def :estate/address          is-string?)
(s/def :estate/location         is-string?)
(s/def :estate/billing-interval (s/and vector? (s/+ :estate/billing-entry)))
(s/def :estate/activity         (s/and vector? (s/+ :estate/activity-entry)))

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

(defn get-activity
	[estate year]
	(apply set/union (map :months (filter #(= (:year %) year) (:activity estate)))))

(defn update-estate-billing
	[estate]
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
	[year estate]
	(let [the-year (filter #(= (:year %) year) (:billing-interval estate))]
		(if (empty? the-year)
			(:months (last (sort-by :year (:billing-interval estate))))
			(:months (first the-year)))))

;;------------------------------------------------------------------------------------

(defn validate-estate
	[house]
	;(println "----------------------------------------\n" "validate-estate:" house "\n----------------------------------------")
	(if (= (s/conform estate-spec house) :clojure.spec/invalid)
		(do
			(error "------- Invalid estate -----------")
			(error (s/explain-str estate-spec house))
			(error "-------------------------------------")
			(error house)
			(throw (Exception. "Invalid estate data")))
		house))

;;------------------------------------------------------------------------------------

(def ^:private config-example
	{:entered    "2017-01-20 12:23:34"
	 :membership {:fee 500 :tax 0.0  :start-year 2017 :start-month 1}
	 :connection {:fee 40  :tax 0.25 :start-year 2017 :start-month 1}
	 :operator   {:fee 90  :tax 0.0  :start-year 2017 :start-month 1}})

(s/def :conf/fee         is-pos-int?)
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

(defn validate-config
	[co]
	(if (= (s/conform config-spec co) :clojure.spec/invalid)
    	(do
    		(error "------- Invalid config -----------")
    		(error (s/explain-str config-spec co))
    		(error "-------------------------------------")
    		(throw (Exception. "Invalid config data")))
    	co))

;;------------------------------------------------------------------------------------

(defn calc-saldo
	[d-c]
	(reduce + (map #(+ (:amount %) (:tax %)) d-c)))
