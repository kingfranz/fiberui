(ns fiberui.core
	(:require [fiberui.frames        :refer :all])
    (:require [fiberui.utils        :refer :all])
    (:require [fiberui.db      :as db  :refer :all])
    (:require [fiberui.data      :as data])
    (:require [seesaw.core        :refer :all])
    (:require [seesaw.border        :refer :all])
    (:require [seesaw.graphics        :refer :all])
    (:require [seesaw.color        :refer :all])
    (:require [seesaw.font        :refer :all])
    (:require [seesaw.dev        :refer :all])
    (:require [taoensso.timbre            :as timbre
             :refer [log  trace  debug  info  warn  error  fatal  report
                     logf tracef debugf infof warnf errorf fatalf reportf spy get-env]])
	(:require [taoensso.timbre.appenders.core :as appenders])
	(:gen-class))

;
; create new member
; edit member
; list all members
; search on name
; search on member-nr
; disable member
; enter payments
;
; create new estate
; edit estate
; list all estates
; disable estate
; search on estate name, address
; enter activities
; enter payments
;
; calculate member fees
; calculate usage fees
; produce invoices
; enter taxrate, what is taxable, usage fees, fastbit fees, membership fees

(defn update-all-members
    []
    (let [all (db/get-all-members)
          all-1 (map #(assoc % :note "" :debit-credit []) all)]
        (doseq [member all-1]
            (data/validate-member member))
        (set-persist false)
        (doseq [memb all-1]
            (db/add-member memb))
        (set-persist true)))

(defn update-all-estates
    []
    (let [all (db/get-all-estates)
          all-1 (map #(assoc % :note ""
                               :debit-credit [] 
                               :billing-interval [{:year 2016 :months 3} {:year 2017 :months 3}]) all)]
        (doseq [estate all-1]
            (data/validate-estate estate))
        (set-persist false)
        (doseq [est all-1]
            (db/add-estate est))
        (set-persist true)))

(defn write-config-entry
    []
    (let [co {:entered    "2017-01-20 12:23:34"
              :membership {:fee 500 :tax 0.0  :start "2017-01-20"}
              :connection {:fee 40  :tax 0.25 :start "2017-01-20"}
              :operator   {:fee 90  :tax 0.0  :start "2017-01-20"}}]
        (data/validate-config co)
        (db/add-config co)))

(defn -main
	[& args]
	(db/db-init)
    ;(update-all-estates)
    (-> (main-frame) (move! :to [500 100]) show!)
    )
