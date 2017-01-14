(ns fiberui.core
	(:require [fiberui.frames        :refer :all])
    (:require [fiberui.utils        :refer :all])
    (:require [fiberui.db      :as db  :refer :all])
    (:require [seesaw.core        :refer :all])
    (:require [seesaw.border        :refer :all])
    (:require [seesaw.graphics        :refer :all])
    (:require [seesaw.color        :refer :all])
    (:require [seesaw.font        :refer :all])
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



(defn -main
	[& args]
	(db/db-init)
	(-> main-frame (move! :to [500 100]) show!))
