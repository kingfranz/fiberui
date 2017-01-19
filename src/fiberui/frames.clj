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

(defn restore-main-frame
	[main-panel]
	(config! (select main-panel [:JMenuItem]) :enabled? true)
	(config! (select main-panel [:JMenu]) :enabled? true)
	(config! (select main-panel [:JMenuBar]) :enabled? true)
	(config! main-panel :content (flow-panel :items [])))

(defn disable-main-menu
	[main-panel]
	(config! (select main-panel [:JMenuItem]) :enabled? false)
	(config! (select main-panel [:JMenu]) :enabled? false)
	(config! (select main-panel [:JMenuBar]) :enabled? false))

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

(defn search-dialog
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
	(search-dialog (->> (db/get-all-estates)
					(sort-by :estate-id)
					(map #(hash-map :id       (:estate-id %)
									:location (:location %)
									:address  (:address %)
									:text     (str (:estate-id %) (:location %) (:address %)))))
				[:id :location :address]))

(defn search-members
	[]
	(search-dialog (->> (db/get-all-members)
					(sort-by :member-id)
					(map #(hash-map :id      (:member-id %)
									:name    (:name %)
									:contact (preferred-contact %)
									:text    (str (:member-id %) (:name %) (preferred-contact %)))))
				[:id :name :contact]))

(defn do-new-member
	[main-panel]
	(let [contact-types {"Adress" :address "E-Post" :email "Telefon" :phone}
		  fnt "ARIAL-BOLD-14"
		  panel (border-panel :border 25 :center (vertical-panel :items [
					(horizontal-panel :items [
						(label :text "Medlemsnummer:" :font fnt)
						[:fill-h 20]
						(text :id :member-id-field :margin 3 :font fnt)])
					[:fill-v 10]

					(horizontal-panel :items [
						(label :text "Namn:" :font fnt)
						[:fill-h 20]
						(text :id :member-name-field :margin 3 :font fnt)])
					[:fill-v 10]

					(horizontal-panel :items [
						(label :text "Startdatum:" :font fnt)
						[:fill-h 20]
						(text :id :member-start-field :margin 3 :font fnt)])
					[:fill-v 30]
					:separator
					
					[:fill-v 10]
					(flow-panel :items [
						(label :text "Fastigheter" :halign :center :font fnt)])
					[:fill-v 10]
					(horizontal-panel :items [
						(button :id :member-estate-field-1 :text "Välj" :font fnt)
						[:fill-h 50]
						(button :id :member-estate-field-2 :text "Välj" :font fnt)
						[:fill-h 50]
						(button :id :member-estate-field-3 :text "Välj" :font fnt)
						[:fill-h 50]
						(button :id :member-estate-field-4 :text "Välj" :font fnt)])
					[:fill-v 30]
					:separator

					[:fill-v 10]
					(flow-panel :items [
						(label :text "Kontakter" :halign :center :font fnt)])
					[:fill-v 10]
					(horizontal-panel :items [
						(combobox :id :member-contact-type-1 :model (vec (keys contact-types)) :font fnt)
						[:fill-h 20]
						(text :id :member-contact-field-1 :margin 3)] :font fnt)
					[:fill-v 10]
					(horizontal-panel :items [
						(combobox :id :member-contact-type-2 :model (vec (keys contact-types)) :font fnt)
						[:fill-h 20]
						(text :id :member-contact-field-2 :margin 3)] :font fnt)
					[:fill-v 10]
					(horizontal-panel :items [
						(combobox :id :member-contact-type-3 :model (vec (keys contact-types)) :font fnt)
						[:fill-h 20]
						(text :id :member-contact-field-3 :margin 3)] :font fnt)
					[:fill-v 10]
					(horizontal-panel :items [
						(combobox :id :member-contact-type-4 :model (vec (keys contact-types)) :font fnt)
						[:fill-h 20]
						(text :id :member-contact-field-4 :margin 3 :font fnt)])
					[:fill-v 30]
					(horizontal-panel :items [
						(button :id :ok-button :text "OK")
						[:fill-h 50]
						(button :id :cancel-button :text "Cancel")])]))

		  get-member-id (fn [] (str/trim (value (select panel [:#member-id-field]))))
		  get-name      (fn [] (str/trim (value (select panel [:#member-name-field]))))
		  get-start     (fn [] (str/trim (value (select panel [:#member-start-field]))))
		  get-contact   (fn [idx] (let [cont-type (->> idx
		  											   (mk-idx-tag "member-contact-type-")
		  											   (select panel)
		  											   selection
		  											   (get contact-types))
		  								cont-val  (->> idx
		  											   (mk-idx-tag "member-contact-field-")
		  											   (select panel)
		  											   value
		  											   str/trim)]
		  						(println "contact:" idx "type:" cont-type "val:" cont-val)
		  							(if (str/blank? cont-val)
		  								nil
		  								(hash-map :type cont-type :value cont-val))))

		  get-estate (fn [idx]  (let [id-val (->> idx (mk-idx-tag "member-estate-field-") (select panel) text)]
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
												 :text (if (nil? ret) "Välj" ret))))]

		(listen (select panel [:#member-estate-field-1]) :action (fn [e] (estate-clicked 1)))
		(listen (select panel [:#member-estate-field-2]) :action (fn [e] (estate-clicked 2)))
		(listen (select panel [:#member-estate-field-3]) :action (fn [e] (estate-clicked 3)))
		(listen (select panel [:#member-estate-field-4]) :action (fn [e] (estate-clicked 4)))
		(listen (select panel [:#ok-button]) :action (fn [e] (if (make-member e)
	                								   			 (restore-main-frame main-panel))))
		(listen (select panel [:#cancel-button]) :action (fn [e] (restore-main-frame main-panel)))

		panel))

(defn do-config
	[main-panel]
	(let [fnt "ARIAL-BOLD-16"
		panel (border-panel :border 50 :center (vertical-panel :items [
		[:fill-v 50]
		(grid-panel :columns 4 :hgap 10 :vgap 10 :items [
		 	(label :halign :center :text "" :font fnt)
		 	(label :halign :center :text "Belopp SEK" :font fnt)
		 	(label :halign :center :text "Moms %" :font fnt)
		 	(label :halign :center :text "Startdatum" :font fnt)

		 	(label :halign :right :text "Medlemsavgift:" :font fnt)
		 	(spinner :id :membership-fee :model (spinner-model 500 :from 0 :to 1000 :by 1.0))
		 	(spinner :id :membership-fee-tax :model (spinner-model 25.0 :from 0.0 :to 99.9 :by 0.1) :tip "0.0 - 99.9")
		 	(text :id :membership-fee-start :tip "Format YYYY-MM-DD" :font fnt)
		 	
		 	(label :halign :right :text "Användningsavgift:" :font fnt)
		 	(spinner :id :connection-fee :model (spinner-model 40 :from 0 :to 1000 :by 1.0))
		 	(spinner :id :connection-fee-tax :model (spinner-model 25.0 :from 0.0 :to 99.9 :by 0.1) :tip "0.0 - 99.9")
		 	(text :id :connection-fee-start :tip "Format YYYY-MM-DD" :font fnt)
		 	
		 	(label :halign :right :text "Operatörsavgift:" :font fnt)
		 	(spinner :id :operator-fee :model (spinner-model 90 :from 0 :to 1000 :by 1.0))
		 	(spinner :id :operator-fee-tax :model (spinner-model 25.0 :from 0.0 :to 99.9 :by 0.1) :tip "0.0 - 99.9")
		 	(text :id :operator-fee-start :tip "Format YYYY-MM-DD" :font fnt)])
		[:fill-v 50]
		(horizontal-panel :items [
		 	(button :id :ok-button :text "OK" :font fnt)
		 	[:fill-h 30]
		 	(button :id :cancel-button :text "Cancel" :font fnt)
		 	])]))

		  get-membership-fee (fn [] {:fee (value (select panel [:#membership-fee]))
		  							 :tax (/ (value (select panel [:#membership-fee-tax])) 100.0)
		  							 :start (value (select panel [:#membership-fee-start]))})
		  get-connection-fee (fn [] {:fee (value (select panel [:#connection-fee]))
		  						:tax (/ (value (select panel [:#connection-fee-tax])) 100.0)
		  						:start (value (select panel [:#connection-fee-start]))})
		  get-operator-fee (fn [] {:fee (value (select panel [:#operator-fee]))
		  						   :tax (/ (value (select panel [:#operator-fee-tax])) 100.0)
		  						   :start (value (select panel [:#operator-fee-start]))})
		  make-config (fn [e] (let [fees {:entered    (utils/now-str)
		  								  :membership (get-membership-fee)
		  								  :connection (get-connection-fee)
		  								  :operator   (get-operator-fee)}]
		  						(if (= (s/conform config-spec fees) :clojure.spec/invalid)
    								(do (alert e (s/explain-str config-spec fees)) false)
    								(do (db/add-config fees) true))))]

		(listen (select panel [:#ok-button]) :action (fn [e]
			(if (make-config e)
				(restore-main-frame main-panel))))
		(listen (select panel [:#cancel-button]) :action (fn [e]
			(restore-main-frame main-panel)))
		panel))

(defn mk-estate-tag
	[est nu]
	(keyword (str (:estate-id est) "-" nu)))

(defn mk-select-tag
	[id nu]
	(keyword (str "#" id "-" nu)))

(defn get-estate-activity
	[estate]
	(if-let [s1 (first (filter #(= (:year %) (utils/year)) (:activity estate)))]
		(:months s1)
		#{}))

(defn activity-frame
	[main-panel]
	(let [mk-boxes (fn [estate]
		  	(let [grid (grid-panel :columns 13 :items (concat [
			  		(button :id (mk-estate-tag estate "button") :text "Alla" :font "ARIAL-BOLD-14")]
			  		(map #(checkbox :id (mk-estate-tag estate (str %))
			  						:text (nth ["Jan" "Feb" "Mar" "Apr" "Maj" "Jun"
			  									"Jul" "Aug" "Sep" "Okt" "Nov" "Dec"] (dec %))
			  						:font "ARIAL-12"
			  						:selected? (contains? (get-estate-activity estate) %))
			  			 (range 1 13))))
		  		  set-box-on (fn [i]
		  			(config! (select grid [(mk-select-tag (:estate-id estate) i)]) :selected? true))]
		  	(listen (select grid [(mk-select-tag (:estate-id estate) "button")]) :action (fn [e]
		  		(doseq [i (range 1 13)] (set-box-on i))))
		  	grid))

		  activity-entry (fn [member]
			(let [estates (db/get-estates-from-member member)
				  houses (mapcat #(vector (label :text (:address %) :halign :left :font "ARIAL-BOLD-16")
				  						  (mk-boxes %)) estates)]
				(grid-panel :vgap 5 :columns 1 :items (concat [
					(label :text (:name member) :halign :left :font "ARIAL-BOLD-16")]
					houses
					[:separator]))))

		  update-entries (fn []
			(let [estate-ids (mapcat :estates (db/get-members-with-estates))
				  is-selected? (fn [id i] (config (select main-panel [(mk-select-tag id i)]) :selected?))
				  mk-selected-set (fn [id] (set (remove nil? (map #(if (is-selected? id %) %) (range 1 13)))))
				  estate-list (map #(hash-map :estate-id % :months (mk-selected-set %)) estate-ids)]
				(db/set-persist false)
				(doseq [list-entry estate-list
						:let [the-estate (db/get-estate (:estate-id list-entry))
							  year (t/year (l/local-now))
							  act-list (:activity the-estate)
							  activities (if act-list
							  				 (remove #(= (:year %) year) act-list)
							  				 [])
							  new-act (conj activities {:year year :months (:months list-entry)})
							  new-estate (assoc the-estate :activity new-act)]]
					(db/add-estate new-estate))
				(db/set-persist true)))

		  activities (fn []
			(let [entries (map activity-entry (db/get-members-with-estates))
				  act   (vertical-panel :items [
							(scrollable (vertical-panel :items entries))
							:separator
							[:fill-v 20]
							(horizontal-panel :items [
								(button :text "OK"
										:listen [:action (fn [e]
											(update-entries)
											(restore-main-frame main-panel)
											)])
								[:fill-h 100]
								(button :text "Cancel"
										:listen [:action (fn [e]
											(restore-main-frame main-panel))])])
							[:fill-v 20]])]
				act))]
	(activities)))

(defn main-frame
	[]
	(let [panel (frame
		:id :main-frame
		:on-close :exit
		:width window-width
		:height window-height
		:menubar (menubar :id :menu-bar :items [
			(menu
				:id :system-menu
				:text "System"
				:items [
					(menu-item :text "Beräkna medlemsavgifter")
					(menu-item :text "Beräkna användningsavgifter")
					(menu-item :text "Skapa fakturor")
					(menu-item :id :config-system :text "Konfigurera")])
			(menu
				:id :member-menu
				:text "Medlemmar"
				:items [
					(menu-item :id :add-member :text "Ny medlem")
					(menu-item :text "Ändra medlem")
					(menu-item :id :member-search :text "Sök medlemmar")
					(menu-item :text "Avregistrera medlem")
					(menu-item :text "Bokför medlemsavgifter")])
			(menu
				:id :estate-menu
				:text "Fastigheter"
				:items [
					(menu-item :text "Ny fastighet")
					(menu-item :text "Ändra fastighet")
					(menu-item :text "Avregistrera fastighet")
					(menu-item :id :estate-search :text "Sök fastighet")
					(menu-item :id :enter-activities :text "Bokför aktiviteter")
					(menu-item :text "Bokför betalningar")])]))
	]
	(listen (select panel [:#config-system]) :action (fn [e]
		(disable-main-menu panel)
		(config! panel :content (do-config panel))))
	
	(listen (select panel [:#add-member]) :action (fn [e]
		(disable-main-menu panel)
		(config! panel :content (do-new-member panel))))
	
	(listen (select panel [:#enter-activities]) :action (fn [e]
		(disable-main-menu panel)
		(config! panel :content (activity-frame panel))))
	
	(listen (select panel [:#member-search]) :action (fn [e]
		(disable-main-menu panel)
		(search-members)
		(restore-main-frame panel)))

	(listen (select panel [:#estate-search]) :action (fn [e]
		(disable-main-menu panel)
		(search-members)
		(restore-main-frame panel)))
	panel))
