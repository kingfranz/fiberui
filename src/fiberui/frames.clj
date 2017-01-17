(ns fiberui.frames
	(:require [fiberui.config       :refer :all])
	(:require [fiberui.db      :as db       :refer :all])
	(:require [fiberui.data      :as data       :refer :all])
	(:require [fiberui.utils      :as utils       :refer :all])
	(:require [clojure.spec               :as s])
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

(defn mk-idx-tag
	[s idx]
	[(keyword (str "#" s idx))])

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

(defn do-dialog
	[data-list columns]
	(let [return-value (atom nil)
		  panel (top-bottom-split
					(horizontal-panel :items [
						(checkbox :id :invert-search :text "Invertera?")
						[:fill-h 20]
						(checkbox :id :and-search :text "And?")
						[:fill-h 20]
						(text :id :search-text :font "ARIAL-BOLD-24")])
					(scrollable
						(table :id :data-table :font "ARIAL-12")
						:vscroll :always
						:hscroll :always)
					:size [640 :by 600]
					:divider-location 30)

		  search-txt       (fn [] (text (select panel [:#search-text])))
		  invert-search    (fn [] (value (select panel [:#invert-search])))
		  and-search       (fn [] (value (select panel [:#and-search])))
		  set-list         (fn [data] (config! (select panel [:#data-table]) :model data))
		  filtered-list    (fn [] (if (str/blank? (search-txt))
									  data-list
									  (filter #(search-words (:text %)
									  						 (search-txt)
									  						 (invert-search)
									  						 (and-search))
									  		  data-list)))
		  update-list (fn [] (set-list [:columns columns
		  								:rows (vec (filtered-list))]))
		  
		  dialog-window (dialog :content panel
	                			:option-type :ok-cancel
	                			:success-fn (fn [e] (if-let [row (selection (select panel [:#data-table]))]
	                									(set-var return-value (:id (nth (filtered-list) row))))
	                								(return-from-dialog e :ok)))]
		(listen (select panel [:#invert-search]) :action (fn [e] (update-list)))
		(listen (select panel [:#and-search]) :action (fn [e] (update-list)))
		(listen (select panel [:#search-text]) #{:insert-update :remove-update} (fn [e] (update-list)))
		(update-list)
		(-> dialog-window pack! show!)
		@return-value))

(defn search-estates
	[]
	(do-dialog (->> (db/get-all-estates)
					(sort-by :estate-id)
					(map #(hash-map :id       (:estate-id %)
									:location (:location %)
									:address  (:address %)
									:text     (str (:estate-id %) (:location %) (:address %)))))
				[:id :location :address]))

(defn search-members
	[]
	(do-dialog (->> (db/get-all-members)
					(sort-by :member-id)
					(map #(hash-map :id      (:member-id %)
									:name    (:name %)
									:contact (preferred-contact %)
									:text    (str (:member-id %) (:name %) (preferred-contact %)))))
				[:id :name :contact]))

(defn do-new-member
	[]
	(let [contact-types {"Adress" :address "E-Post" :email "Telefon" :phone}
		  panel (vertical-panel :items [
					(horizontal-panel :items [
						(label "Medlemsnummer:") [:fill-h 20]
						(text :id :member-id-field :margin 3 :halign :right)
						[:fill-h 300]])
					[:fill-v 10]

					(horizontal-panel :items [
						(label "Namn:") [:fill-h 20]
						(text :id :member-name-field :margin 3 :columns 50)
						[:fill-h 300]])
					[:fill-v 10]

					(horizontal-panel :items [
						(label "Startdatum:") [:fill-h 20]
						(text :id :member-start-field :margin 3)])
					[:fill-v 30]
					:separator
					
					[:fill-v 10]
					(label "Fastigheter:")
					[:fill-v 10]
					(horizontal-panel :items [
						(button :id :member-estate-field-1 :text "Välj")
						[:fill-h 50]
						(button :id :member-estate-field-2 :text "Välj")
						[:fill-h 50]
						(button :id :member-estate-field-3 :text "Välj")
						[:fill-h 50]
						(button :id :member-estate-field-4 :text "Välj")])
					[:fill-v 30]
					:separator

					[:fill-v 10]
					(label :text "Kontakter:")
					[:fill-v 10]
					(horizontal-panel :items [
						(combobox :id :member-contact-type-1 :model (vec (keys contact-types)))
						[:fill-h 20]
						(text :id :member-contact-field-1 :margin 3)])
					[:fill-v 10]
					(horizontal-panel :items [
						(combobox :id :member-contact-type-2 :model (vec (keys contact-types)))
						[:fill-h 20]
						(text :id :member-contact-field-2 :margin 3)])
					[:fill-v 10]
					(horizontal-panel :items [
						(combobox :id :member-contact-type-3 :model (vec (keys contact-types)))
						[:fill-h 20]
						(text :id :member-contact-field-3 :margin 3)])
					[:fill-v 10]
					(horizontal-panel :items [
						(combobox :id :member-contact-type-4 :model (vec (keys contact-types)))
						[:fill-h 20]
						(text :id :member-contact-field-4 :margin 3)])])

		  get-member-id (fn [] (str/trim (value (select panel [:#member-id-field]))))
		  get-name      (fn [] (str/trim (value (select panel [:#member-name-field]))))
		  get-start     (fn [] (str/trim (value (select panel [:#member-start-field]))))
		  get-contact   (fn [idx] (let [cont-type (->> idx (mk-idx-tag "member-contact-type-") (select panel) selection (get contact-types))
		  								cont-val  (->> idx (mk-idx-tag "member-contact-field-") (select panel) value str/trim)]
		  						(println "contact:" idx "type:" cont-type "val:" cont-val)
		  							(if (str/blank? cont-val)
		  								nil
		  								(hash-map :type cont-type :value cont-val))))

		  get-estate (fn [idx]  (let [id-val (->> idx (mk-idx-tag "member-estate-field-") (select panel) text)]
		  	(println "estate:" idx "val:" id-val)
		  							(if (or (str/blank? id-val) (= id-val "Välj"))
		  								nil
		  								id-val)))
		  member-id-ok? (fn [] (and (is-pos-int-str? (get-member-id))
		  							(not (db/member-id-exist? (utils/parse-int (get-member-id))))))
		  member-name-ok? (fn [] (utils/not-blank? (get-name)))
		  member-start-ok? (fn [] (utils/date? (get-start)))
		  contact-ok? (fn [] (utils/is-string? (value (select panel [:#member-contact-field-1]))))
		  member-ctor (fn [] (hash-map :member-id (utils/parse-int (get-member-id))
		  							   :name (get-name)
		  							   :contact (vec (remove nil? [(assoc (get-contact 1) :preferred true)
		  							   							   (get-contact 2)
		  							   			 				   (get-contact 3)
		  							   			 				   (get-contact 4)]))
		  							   :from-to (hash-map :from (get-start))
		  							   :estates (vec (remove nil? [(get-estate 1)
		  							   							   (get-estate 2)
		  							   			 				   (get-estate 3)
		  							   			 				   (get-estate 4)]))))
		  make-member (fn [e] (if (not (member-id-ok?))
		  						(do (alert e "Fel medlems#") false)
		  						(if (not (member-name-ok?))
		  							(do (alert e "Ogiltigt namn") false)
		  							(if (not (member-start-ok?))
		  								(do (alert e "Ogiltigt startdatum") false)
		  								(if (not (contact-ok?))
		  									(do (alert e "Ogiltig kontakt") false)
		  									(let [memb (member-ctor)]
		  										(println memb)
		  										(if (= (s/conform member-spec memb) :clojure.spec/invalid)
    												(do (alert e (s/explain-str member-spec memb)) false)
    												(do (db/add-member memb) true))))))))
		  estate-clicked (fn [idx]  (let [ret (search-estates)]
										(config! (select panel (mk-idx-tag "member-estate-field-" idx))
												 :text (if (nil? ret) "Välj" ret))))
		  
		  dialog-window (dialog :content panel
		  						:title "Ny medlem"
	                			:options [(button :text "OK"
	                							  :listen [:action (fn [e] (if (make-member e)
	                														   (return-from-dialog e :ok)))])
	                					 (button :text "Cancel"
	                							 :listen [:action (fn [e] (return-from-dialog e :cancel))])])]

		(listen (select panel [:#member-estate-field-1]) :action (fn [e] (estate-clicked 1)))
		(listen (select panel [:#member-estate-field-2]) :action (fn [e] (estate-clicked 2)))
		(listen (select panel [:#member-estate-field-3]) :action (fn [e] (estate-clicked 3)))
		(listen (select panel [:#member-estate-field-4]) :action (fn [e] (estate-clicked 4)))

		(-> dialog-window pack! show!)))

(defn testa
	[]
	(println "RET:" (show! (dialog :content (button :text "aaa")
			:option-type :ok-cancel
	        :success-fn (fn [e] (return-from-dialog e :ok))))))

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
					(menu-item :text "Beräkna medlemsavgifter" :listen [:action (fn [e] (testa))])
					(menu-item :text "Beräkna användningsavgifter")
					(menu-item :text "Skapa fakturor")
					(menu-item :text "Konfigurera")])
			(menu
				:id :member-menu
				:text "Medlemmar"
				:items [
					(menu-item :text "Ny medlem" :listen [:action (fn [e] (do-new-member))])
					(menu-item :text "Ändra medlem")
					(menu-item :text "Sök medlemmar" :listen [:action (fn [e] (search-members))])
					(menu-item :text "Avregistrera medlem")
					(menu-item :text "Bokför medlemsavgifter")])
			(menu
				:id :estate-menu
				:text "Fastigheter"
				:items [
					(menu-item :text "Ny fastighet")
					(menu-item :text "Ändra fastighet")
					(menu-item :text "Avregistrera fastighet")
					(menu-item :text "Sök fastighet" :listen [:action (fn [e] (search-estates))])
					(menu-item :text "Bokför aktiviteter")
					(menu-item :text "Bokför betalningar")])])))
