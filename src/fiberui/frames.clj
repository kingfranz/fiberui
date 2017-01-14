(ns fiberui.frames
	(:require [fiberui.config       :refer :all])
	(:require [fiberui.db      :as db       :refer :all])
	(:require [fiberui.data      :as data       :refer :all])
	(:require [clj-time.core              :as t])
	(:require [clj-time.format            :as f])
	(:require [clj-time.local             :as l])
	(:require [seesaw.core        :refer :all])
    (:require [seesaw.border        :refer :all])
    (:require [seesaw.graphics        :refer :all])
    (:require [seesaw.color        :refer :all])
    (:require [seesaw.font        :refer :all])
    (:require [taoensso.timbre            :as timbre
             :refer [log  trace  debug  info  warn  error  fatal  report
                     logf tracef debugf infof warnf errorf fatalf reportf spy get-env]])
	(:require [taoensso.timbre.appenders.core :as appenders])
	(:require [clojure.set    :refer [superset?]])
	(:require [clojure.string  :as str  :refer [includes?]])
	)

;;------------------------------------------------------------------------------------

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

(def create-member-frame
	(vertical-panel :id :create-member-frame :items [
		(horizontal-panel :items [
			(label "Medlemsnummer:")
			(text :id :member-id-field)])
		(horizontal-panel :items [
			(label "Namn:")
			(text :id :member-name-field)])
		(horizontal-panel :items [
			(label "Startdatum:")
			(text :id :member-start-field)]) ; (date-input )
		(horizontal-panel :items [
			(label "Fastigheter:")
			(text :id :member-estates-field)])
		(horizontal-panel :items [
			(label "Kontakter:")
			(text :id :member-contact-field)])
		(horizontal-panel :items [
			(button :id :member-cancel-btn :text "Avbryt")
			(button :id :member-ok-btn :text "Spara")])]))

(declare get-filtered-members)

(defn list-members-panel
	[]
	(border-panel
		;:width window-width
		;:height window-height
		:north
			(horizontal-panel
				;:height 100
				:items [
					(checkbox :id :invert-members :text "Invertera?")
					(text :id :search-text :columns 50 :listen [:key-pressed (fn [e] (println e))])])
		:center
			(listbox :id :member-listbox :model [])))

(defn set-listbox
	[panel box-id data]
	(config! (select panel [box-id]) :model data))

(defn search-words
	[txt search-txt]
	(some true? (map #(str/includes? txt %) (str/split search-txt #" +"))))

(defn get-filtered-members
	[panel]
	(let [members (db/get-all-members)
		  member-txt (map data/mk-member-str members)
		  search-txt (text (select panel [:#search-text]))]
		(if (str/blank? search-txt)
			member-txt
			(filter #(search-words % search-txt) member-txt))))

(defn menu-handler
	[event id]
	(println "event:" event " id:" id)
	(println "action:" (.getActionCommand event)))

(defn do-dialog
	[e mk-panel]
	(let [panel (mk-panel)
		  a-panel (set-listbox panel :#member-listbox (get-filtered-members panel))
		  dialog-window (dialog :width 500 :height 400
		  						:content a-panel
	                			:option-type :ok-cancel
	                			:success-fn (fn [p] (text (select (to-root p) [:#name]))))]
	(show! dialog-window)))

(def main-frame
	(frame
		:id :main-frame
		:on-close :exit
		:width window-width
		:height window-height
		:menubar (menubar :items [
			(menu
				:id :system-menu
				:text "System"
				:items [
					(menu-item :id :calc-member-fees :text "Beräkna medlemsavgifter")
					(menu-item :id :calc-usage-fees :text "Beräkna användningsavgifter")
					(menu-item :id :make-invoices :text "Skapa fakturor")
					(menu-item :id :configurate :text "Konfigurera")])
			(menu
				:id :member-menu
				:text "Medlemmar"
				:items [
					(menu-item :text "Ny medlem" :listen [:action (fn [e] (menu-handler e :new-member))])
					(menu-item :text "Ändra medlem" :listen [:action (fn [e] (menu-handler e :edit-member))])
					(menu-item :text "Lista medlemmar" :listen [:action (fn [e] (do-dialog e list-members-panel))])
               
					(menu-item :id :search-member :text "Sök medlem")
					(menu-item :id :disable-member :text "Avregistrera medlem")
					(menu-item :id :payment-member :text "Bokför medlemsavgifter")])
			(menu
				:id :estate-menu
				:text "Fastigheter"
				:items [
					(menu-item :id :new-estate :text "Ny fastighet")
					(menu-item :id :edit-estate :text "Ändra fastighet")
					(menu-item :id :list-estates :text "lista fastigheter")
					(menu-item :id :disable-estate :text "Avregistrera fastighet")
					(menu-item :id :search-estate :text "Sök fastighet")
					(menu-item :id :edit-activities :text "Bokför aktiviteter")
					(menu-item :id :payment-estates :text "Bokför betalningar")])])))
