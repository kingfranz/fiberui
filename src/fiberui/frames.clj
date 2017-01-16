(ns fiberui.frames
	(:require [fiberui.config       :refer :all])
	(:require [fiberui.db      :as db       :refer :all])
	(:require [fiberui.data      :as data       :refer :all])
	(:require [fiberui.utils      :as utils       :refer :all])
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
; search members
; disable member
; enter payments
;
; create new estate
; edit estate
; disable estate
; search on estate name, address
; enter activities
; enter payments
;
; calculate member fees
; calculate usage fees
; produce invoices
; enter taxrate, what is taxable, usage fees, fastbit fees, membership fees

(defn do-new-member
	[]
	(let [panel (vertical-panel :items [
					(horizontal-panel :items [
						(label "Medlemsnummer:") [:fill-h 20]
						(text :id :member-id-field :margin 3 :halign :right)
						[:fill-h 300]])
					[:fill-v 10]

					(horizontal-panel :items [
						(label "Namn:") [:fill-h 20]
						(text :id :member-name-field :margin 3 :columns 50)])
					[:fill-v 10]

					(horizontal-panel :items [
						(label "Startdatum:") [:fill-h 20]
						(text :id :member-start-field :margin 3)]) ; (date-input )
					[:fill-v 30]
					:separator
					
					[:fill-v 10]
					(label "Fastigheter:")
					[:fill-v 10]
					(horizontal-panel :items [
						(button :id :member-estate-field-1 :text "Välj") [:fill-h 50]
						(button :id :member-estate-field-2 :text "Välj") [:fill-h 50]
						(button :id :member-estate-field-3 :text "Välj") [:fill-h 50]
						(button :id :member-estate-field-4 :text "Välj")])
					[:fill-v 30]
					:separator

					[:fill-v 10]
					(label :text "Kontakter:")
					[:fill-v 10]
					(horizontal-panel :items [
						(combobox :id :member-contact-type-1 :model ["Adress" "E-Post" "Telefon"])
						[:fill-h 20]
						(text :id :member-contact-field-1 :margin 3)])
					[:fill-v 10]
					(horizontal-panel :items [
						(combobox :id :member-contact-type-2 :model ["Adress" "E-Post" "Telefon"])
						[:fill-h 20]
						(text :id :member-contact-field-2 :margin 3)])
					[:fill-v 10]
					(horizontal-panel :items [
						(combobox :id :member-contact-type-3 :model ["Adress" "E-Post" "Telefon"])
						[:fill-h 20]
						(text :id :member-contact-field-3 :margin 3)])
					[:fill-v 10]
					(horizontal-panel :items [
						(combobox :id :member-contact-type-4 :model ["Adress" "E-Post" "Telefon"])
						[:fill-h 20]
						(text :id :member-contact-field-4 :margin 3)])])

		  get-member-id (fn [] (str/trim (value (select panel [:#member-id-field]))))
		  get-name      (fn [] (str/trim (value (select panel [:#member-name-field]))))
		  get-start     (fn [] (str/trim (value (select panel [:#member-start-field]))))
		  member-id-ok? (fn [] (and (is-pos-int-str? (get-member-id))
		  							(not (db/member-id-exist? (get-member-id)))))
		  member-name-ok? (fn [] (utils/not-blank? (get-name)))
		  member-start-ok? (fn [] (and (utils/not-blank? (get-start))
		  							   (utils/date? (get-start))))
		  
		  dialog-window (dialog :content panel
		  						:title "Ny medlem"
	                			:options [(button :text "OK"
	                							  :listen [:action (fn [e] (if (and (member-id-ok?)
	                							  									(member-name-ok?)
	                							  									(member-start-ok?))
	                														   (return-from-dialog e :ok)
	                														   (alert e "Fel medlems#")))])
	                					 (button :text "Cancel"
	                							 :listen [:action (fn [e] (return-from-dialog e :cancel))])])]
		(-> dialog-window pack! show!)))


(defn search-words
	[txt search-txt invert-search and-search]
	(let [match-list (map #(str/includes? (str/lower-case txt) %) (str/split (str/lower-case search-txt) #" +"))]
		(if invert-search
			(if and-search
				(not (not (some false? match-list)))
				(not (some true? match-list)))
			(if and-search
				(not (some false? match-list))
				(some true? match-list)))))

(defn do-list-members
	[]
	(let [member-data (db/get-all-members)
		  members     (sort-by :member-id member-data)
		  member-map  (map #(hash-map :id (:member-id %) :name (:name %) :contact (preferred-contact %)) members)

		  panel (top-bottom-split
					(horizontal-panel :items [
						(checkbox :id :invert-search :text "Invertera?")
						[:fill-h 20]
						(checkbox :id :and-search :text "And?")
						[:fill-h 20]
						(text :id :search-text :font "ARIAL-BOLD-24")])
					(scrollable
						(table :id :member-list :font "ARIAL-12")
						:vscroll :always
						:hscroll :always)
					:size [640 :by 600]
					:divider-location 30)

		  search-txt       (fn [] (text (select panel [:#search-text])))
		  invert-search    (fn [] (value (select panel [:#invert-search])))
		  and-search       (fn [] (value (select panel [:#and-search])))
		  set-list         (fn [data] (config! (select panel [:#member-list]) :model data))
		  filtered-members (fn [] (if (str/blank? (search-txt))
									  member-map
									  (filter #(search-words (data/mk-member-str %)
									  						 (search-txt)
									  						 (invert-search)
									  						 (and-search))
									  		  member-map)))
		  update-list (fn [] (set-list [:columns [:id :name :contact]
		  								:rows (vec (filtered-members))]))
		  
		  dialog-window (dialog :content panel
	                			:option-type :default)]
		(listen (select panel [:#invert-search]) :action (fn [e] (update-list)))
		(listen (select panel [:#and-search]) :action (fn [e] (update-list)))
		(listen (select panel [:#search-text]) #{:insert-update :remove-update} (fn [e] (update-list)))
		(update-list)
		;(println (count member-data) (count members) (count member-txt))
		(request-focus! (select panel [:#search-text]))
		(-> dialog-window pack! show!)))

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
					(menu-item :text "Beräkna medlemsavgifter")
					(menu-item :text "Beräkna användningsavgifter")
					(menu-item :text "Skapa fakturor")
					(menu-item :text "Konfigurera")])
			(menu
				:id :member-menu
				:text "Medlemmar"
				:items [
					(menu-item :text "Ny medlem" :listen [:action (fn [e] (do-new-member))])
					(menu-item :text "Ändra medlem")
					(menu-item :text "Sök medlemmar" :listen [:action (fn [e] (do-list-members))])
					(menu-item :text "Avregistrera medlem")
					(menu-item :text "Bokför medlemsavgifter")])
			(menu
				:id :estate-menu
				:text "Fastigheter"
				:items [
					(menu-item :text "Ny fastighet")
					(menu-item :text "Ändra fastighet")
					(menu-item :text "Avregistrera fastighet")
					(menu-item :text "Sök fastighet")
					(menu-item :text "Bokför aktiviteter")
					(menu-item :text "Bokför betalningar")])])))
