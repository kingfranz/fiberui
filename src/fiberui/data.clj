(ns fiberui.data
  (:require [fiberui.utils      :as utils      :refer :all])
  (:require [clojure.spec               :as s])
  (:require [clj-time.core              :as t])
  (:require [clj-time.format            :as f])
  (:require [clj-time.local             :as l])
  (:require [taoensso.timbre            :as timbre
             :refer [log  trace  debug  info  warn  error  fatal  report
                     logf tracef debugf infof warnf errorf fatalf reportf spy get-env]])
  (:require [taoensso.timbre.appenders.core :as appenders])
  (:require [clojure.set    :refer [superset?]]))


;;------------------------------------------------------------------------------------

(defn mk-email1-str
    [contact]
    (let [email1 (filter #(and (= (:type %) :email) (:preferred %)) contact)]
        (if (empty? email1)
            ""
            (:value email1))))

(defn preferred-contact
	[member]
	(if-let [pref (filter #(:preferred %) (:contact member))]
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
	 :debit-credit  [{:date "2016-12-5" :amount 500 :type :membership-fee :note ""}]
	 :from-to       {:from "2016-12-5" :to "2016-12-5"}
	 :estates       ["MBEF82"]
	 :note          ""})

;;------------------------------------------------------------------------------------
; [[:addr {:value aaaaaaaa, :type :address, :preferred true}] [:addr {:value bbbbbbbbb, :type :address}]]

(defn contains-preferred?
	[contacts]
	;(println "contains:" contacts)
	(->> contacts (map second) (filter :preferred) not-empty))

(s/def :fiber/date           date?)
(s/def :fiber/amount         number?)
(s/def :fiber/from           date?)
(s/def :fiber/to             date?)
(s/def :fiber/from-to        (s/keys :req-un [:fiber/from] :opt-un [:fiber/to]))
(s/def :fiber/note           is-string?)
(s/def :fiber/type           #{:membership-fee})
(s/def :fiber/dc-entry       (s/keys :req-un [:fiber/date :fiber/amount :fiber/type] :opt-un [:fiber/note]))
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

(s/def :member/name          is-string?)
(s/def :member/contact       (s/and (s/+ :member/contact-entry)
									contains-preferred?))
(s/def :member/member-id     is-pos-int?)
(s/def :member/estates       (s/+ is-estate-id?))
(s/def :fiber/debit-credit   (s/+ :fiber/dc-entry))

;;------------------------------------------------------------------------------------

(def member-spec (s/keys :req-un [:member/member-id
								  :member/name
                               	  :member/contact
                               	  :fiber/from-to]
                         :opt-un [:fiber/debit-credit
                         		  :fiber/note
                               	  :member/estates]))

;;------------------------------------------------------------------------------------

(defn mk-estate-str
	[estate]
	(str "ID: " (:estate-id estate) " " (:location estate) " " (:address estate)))

(def ^:private estate-example
	{:estate-id        "MBEF82"
	 :location         "Myrhult 1:2"
	 :address          "Lindåsen Höjen 52, 54592 Älgarås"
	 :debit-credit     [{:date "2016-12-5" :amount 500 :type :usage-fee :note ""}]
	 :billing-interval {:current {:from "2016-01-01" :to "2016-12-31" :months 3}
						:next    {:from "2016-01-01" :to "2016-12-31" :months 3}}
	 :activity         [{:year 2016 :months #{1 2 3 10 11 12}}]
	 :note             ""})

;;------------------------------------------------------------------------------------

(s/def :estate/year             int?)
(s/def :billing/months          int?)
(s/def :billing/current         (s/keys :req-un [:fiber/from :fiber/to :billing/months]))
(s/def :billing/next            (s/keys :req-un [::from ::to :billing/months]))
(s/def :estate/months           (s/and set? #(superset? #{1 2 3 4 5 6 7 8 9 10 11 12} %)))
(s/def :estate/activity-entry   (s/keys :req-un [:estate/year :estate/months]))

(s/def :estate/address          is-string?)
(s/def :estate/location         is-string?)
(s/def :estate/billing-interval (s/keys :req-un [:billing/current :billing/next]))
(s/def :estate/activity         (s/* :estate/activity-entry))
(s/def :estate/estate-id        is-estate-id?)

;;------------------------------------------------------------------------------------

(def estate-spec (s/keys :req-un [:estate/estate-id
									:estate/location
									:estate/address]
                           :opt-un [:fiber/note
                           			:fiber/debit-credit
                                    :estate/billing-interval
                                    :estate/activity]))

;;------------------------------------------------------------------------------------

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

;;------------------------------------------------------------------------------------

(defn validate-estate
  [house]
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
	 :membership {:fee 500 :tax 0.0  :start "2017-01-20"}
	 :connection {:fee 40  :tax 0.25 :start "2017-01-20"}
	 :operator   {:fee 90  :tax 0.0  :start "2017-01-20"}})

(s/def :conf/fee       is-pos-int?)
(s/def :conf/tax       (s/and double? #(>= % 0.0) #(< % 1.0)))
(s/def :conf/start     #(instance? org.joda.time.DateTime %))
(s/def :conf/fee-entry (s/keys :req-un [:conf/fee :conf/tax :conf/start]))

(s/def :conf/entered    #(instance? org.joda.time.DateTime %))
(s/def :conf/membership :conf/fee-entry)
(s/def :conf/connection :conf/fee-entry)
(s/def :conf/operator   :conf/fee-entry)

(def config-spec (s/keys :req-un [:conf/entered
								  :conf/membership
								  :conf/connection
								  :conf/operator]))
