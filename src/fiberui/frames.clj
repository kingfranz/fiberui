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
    (:require [seesaw.forms        :as forms])
    (:require [seesaw.font        :refer :all])
    (:require [clojure.data.csv :as csv])
    (:require [clojure.java.io :as io])
    (:require [taoensso.timbre            :as timbre
             :refer [log  trace  debug  info  warn  error  fatal  report
                     logf tracef debugf infof warnf errorf fatalf reportf spy get-env]])
	(:require [taoensso.timbre.appenders.core :as appenders])
	(:require [clojure.set     :as set :refer [superset? intersection union]])
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
	[data-list columns export-title export-cols]
	(let [return-value (atom nil)
		  panel (forms/forms-panel
              "right:40dlu,5dlu,20dlu,5dlu,right:20dlu,5dlu,300dlu"
              :default-row-spec (com.jgoodies.forms.layout.RowSpec. "20px")
              :leading-column-offset 0
              :default-dialog-border? true
              :line-gap-size (com.jgoodies.forms.layout.Sizes/pixel 10)
              :items   ["Invertera?" (checkbox :id :invert-search)
              			"And?" (checkbox :id :and-search)
              			(text :id :search-text :font "ARIAL-BOLD-18")
              			(forms/separator)
                      	(forms/span (scrollable	(table :id :data-table :font "ARIAL-12")
							:vscroll :always
							:hscroll :always) 6)])

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
		  export-row (fn [cols row] (vec (map #(get row %) cols)))
		  
		  dialog-window (dialog :content panel
		  	:width 1000
		  	:height 700
		  	:title "a title"
	                			:options [
	                		(button :id :ok-button
    							  :text "OK"
    							  :listen [:action (fn [e]
    							  	  (if-let [row (selection (select panel [:#data-table]))]
	                					(set-var return-value (:id (nth (filtered-list) row))))
    								  (return-from-dialog e :ok))])
    					  (button :id :cancel-button
    					  		  :text "Cancel"
    					  		  :listen [:action (fn [e]
    					  			  (return-from-dialog e :cancel))])
    					  (button :id :export-button
    					  		  :text "Export"
    					  		  :listen [:action (fn [e]
    					  		  	  	(with-open [out-file (io/writer "out-file.csv")]
											(csv/write-csv out-file
						                		(concat export-title (map #(export-row export-cols %)
						                								  (filtered-list)))))
										(alert (str "Exporterade till: " "out-file.csv")))])])]

		(listen (select panel [:#invert-search]) :action (fn [e]
			(update-list)))
		(listen (select panel [:#and-search]) :action (fn [e]
			(update-list)))
		(listen (select panel [:#search-text]) #{:insert-update :remove-update}
			(fn [e] (update-list)))
		(update-list)
		(-> dialog-window show!)
		@return-value))

(defn search-estates
	[]
	(search-dialog (->> (db/get-all-estates)
					(sort-by :estate-id)
					(map #(hash-map :id       (:estate-id %)
									:location (:location %)
									:address  (:address %)
									:text     (str (:estate-id %) (:location %) (:address %)))))
				[:id :location :address]
				[["Fastigheter" (utils/now-str) ""]
				 ["ID" "Betäckning" "Adress"]]
				[:id :location :address]))

(defn search-members
	[]
	(search-dialog (->> (db/get-all-members)
					(sort-by :member-id)
					(map #(hash-map :id      (:member-id %)
									:namn    (:name %)
									:kontakt (preferred-contact %)
									:text    (str (:member-id %) (:name %) (preferred-contact %)))))
				[:id :namn :kontakt]
				[["Medlemmar" (utils/now-str) ""]
				 ["ID" "Namn" "Kontakt"]]
				[:id :namn :kontakt]))

(defn list-invoice-members
	[year]
	(search-dialog (->> (db/get-all-members)
						(map #(hash-map :medlemsnr      (:member-id %)
										:name    (:name %)
										:kontact (data/preferred-contact %)
										:belopp  (data/sum-debit-credit (:member-id %) year % [:membership-fee])
								        :text    (str (:member-id %) (:name %) (data/preferred-contact %))))
						(remove #(zero? (:belopp %)))
						(sort-by :medlemsnr))
				[:medlemsnr :name :kontact :belopp]
				[["Skulder för medlemsavgift" (utils/now-str) "" ""]
				 ["Medlemsnummer" "Namn" "Kontakt" "Belopp"]]
				[:medlemsnr :namn :kontact :belopp]))

(defn list-invoice-usage
	[year yearly]
	(let [mk-pay (fn [x] (let [members  (db/get-owners-from-estate year x)
							   con-fee (fn [id] (data/sum-debit-credit id year x [:connection-fee]))
							   op-fee  (fn [id] (data/sum-debit-credit id year x [:operator-fee]))]
							(map #(hash-map :fastighet (:address x)
								      :medlemsnr (:member-id %)
								      :namn (:name %)
								      :kontakt (preferred-contact %)
								      :belopp (format "a: %.2f o: %.2f :s %.2f"
								      				  (con-fee (:member-id %))
								      				  (op-fee (:member-id %))
								      				  (+ (con-fee (:member-id %))
								      				  	 (op-fee (:member-id %))))
								      :tot-amount (+ (con-fee (:member-id %))
								      				 (op-fee (:member-id %)))
								      :conn-fee (con-fee (:member-id %))
								      :oper-fee (op-fee (:member-id %))
								      :text (str (:address x)
								      			 (:member-id %)
								      			 (:name %)
								      			 (preferred-contact %))) members)))]
	(search-dialog (->> (db/get-all-estates)
						(filter #(if yearly (data/is-yearly-bill year %) (not (data/is-yearly-bill year %))))
						(mapcat mk-pay)
						(filter #(not (zero? (:tot-amount %))))
						(sort-by :medlemsnr))
				[:medlemsnr :namn :fastighet :kontakt :belopp]
				[["Skulder för anslutning och andvändning" (utils/now-str) "" "" "" "" ""]
				 ["Medlemsnummer" "Namn" "Fastighet" "Kontakt" "Anslutninsavgift" "Användningsavgift" "Totalt"]]
				[:medlemsnr :namn :fastighet :kontakt :conn-fee :oper-fee :tot-amount])))

(defn do-new-member
	[main-panel]
	(let [contact-types {"Adress" :address "E-Post" :email "Telefon" :phone}
		  fnt "ARIAL-BOLD-14"
		  panel (forms/forms-panel
              "right:pref,8dlu,50dlu,8dlu,100dlu"
              :default-row-spec (com.jgoodies.forms.layout.RowSpec. "20px")
              :leading-column-offset 0
              :default-dialog-border? true
              :line-gap-size (com.jgoodies.forms.layout.Sizes/pixel 10)
              :items   [(forms/title "Skapa ny medlem") (forms/next-line)
                      	(forms/separator "General")
                      	"Medlemsnr" (spinner :id :member-id-nr :font fnt
		 			 			 :model (spinner-model 100 :from 1 :to 1000 :by 1))
                      		(forms/next-line)
                      	"Namn" (forms/span (text :id :member-name-field) 3) (forms/next-line)
                      	"Notering" (forms/span (text :id :member-note-field) 3) (forms/next-line)
                      	(forms/separator "Medlemskapet börjar")
                      	"år" (spinner :id :member-start-year :font fnt
		 			 			 :model (spinner-model (long (utils/year)) :from 2010 :to 2020 :by 1))
                      		(forms/next-line)
                      	"Månad" (spinner :id :member-start-month :font fnt
		 			 			 :model (spinner-model (long (utils/month)) :from 1 :to 12 :by 1))
                      		(forms/next-line)
                      	(forms/separator "Fastighet")
                      	(forms/span (button :id :estate-btn :text "Välj") 1)
                      		(forms/span (text :id :estate-name) 1)
                      		(forms/next-line)
                      	(forms/separator "Kontakter (den första blir faktureringsadress)")
                      	(combobox :id :member-contact-type-1 :model (vec (keys contact-types)))
                      		(forms/span (text :id :member-contact-field-1) 3)
                      		(forms/next-line)
                      	(combobox :id :member-contact-type-2 :model (vec (keys contact-types)))
                      		(forms/span (text :id :member-contact-field-2) 3)
                      		(forms/next-line)
                      	(combobox :id :member-contact-type-3 :model (vec (keys contact-types)))
                      		(forms/span (text :id :member-contact-field-3) 3)
                      		(forms/next-line)
                      	(combobox :id :member-contact-type-4 :model (vec (keys contact-types)))
                      		(forms/span (text :id :member-contact-field-4) 3)
                      		(forms/next-line)
                      	(forms/separator)
                      	(forms/span (button :id :ok-button :text "OK") 1)
                      	(forms/span (button :id :cancel-button :text "Cancel") 1)])

		  get-member-id (fn [] (value (select panel [:#member-id-nr])))
		  get-name      (fn [] (str/trim (value (select panel [:#member-name-field]))))
		  get-note      (fn [] (str/trim (value (select panel [:#member-note-field]))))
		  get-start     (fn [] (format "%d-%d-1" (value (select panel [:#member-start-year]))
		  										 (value (select panel [:#member-start-month]))))
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
		  							(if (str/blank? cont-val)
		  								nil
		  								(hash-map :type cont-type :value cont-val))))

		  get-estate-name (fn [] (str/trim (value (select panel [:#estate-name]))))
		  member-ctor (fn [] {:member-id (get-member-id)
		  					  :name (get-name)
		  					  :note (get-note)
		  					  :contact (vec (remove nil? [(assoc (get-contact 1) :preferred true)
		  						   							     (get-contact 2)
		  							   			 				 (get-contact 3)
		  							   			 				 (get-contact 4)]))
		  					  :from-to {:from (get-start)}
		  					  :estates (if-not (utils/is-estate-id? (get-estate-name))
		  					   				[]
		  					   				[{:estate-id (get-estate-name) :from-to {:from (get-start)}}])})
		  make-member (fn [e] (if (= (s/conform member-spec (member-ctor)) :clojure.spec/invalid)
    							(do (alert e (s/explain-str member-spec (member-ctor))) false)
    							(do (db/add-member (member-ctor)) true)))
		  estate-clicked (fn [idx]  (let [ret (search-estates)]
										(config! (select panel [:#estate-name])
												 :text (if (nil? ret) "" ret))))]

		(listen (select panel [:#estate-btn]) :action (fn [e] (estate-clicked 1)))
		(listen (select panel [:#ok-button]) :action (fn [e] (if (make-member e)
	                								   			 (restore-main-frame main-panel))))
		(listen (select panel [:#cancel-button]) :action (fn [e] (restore-main-frame main-panel)))

		panel))

(defn do-new-estate
	[main-panel]
	(let [fnt "ARIAL-BOLD-14"
		  panel (forms/forms-panel
              "right:pref,8dlu,50dlu,8dlu,100dlu"
              :default-row-spec (com.jgoodies.forms.layout.RowSpec. "20px")
              :leading-column-offset 0
              :default-dialog-border? true
              :line-gap-size (com.jgoodies.forms.layout.Sizes/pixel 10)
              :items   [(forms/title "Skapa ny anslutning") (forms/next-line)
                      	(forms/separator "General")
                      	"Projektnummer" (forms/span (text :id :estate-id-field) 3) (forms/next-line)
                      	"Location" (forms/span (text :id :estate-loc-field) 3) (forms/next-line)
                      	"Adress" (forms/span (text :id :estate-address-field) 3) (forms/next-line)
                      	"Note" (forms/span (text :id :estate-note-field) 3) (forms/next-line)
                      	(forms/separator "Anslutningen börjar")
                      	"år" (spinner :id :estate-start-year :font fnt
		 			 			 :model (spinner-model (long (utils/year)) :from 2010 :to 2020 :by 1))
                      		(forms/next-line)
                      	"Månad" (spinner :id :estate-start-month :font fnt
		 			 			 :model (spinner-model (long (utils/month)) :from 1 :to 12 :by 1))
                      		(forms/next-line)
                      	(forms/separator)
                      	(forms/span (button :id :ok-button :text "OK") 1)
                      	(forms/span (button :id :cancel-button :text "Cancel") 1)])


		  get-estate-id (fn [] (str/trim (value (select panel [:#estate-id-field]))))
		  get-loc       (fn [] (str/trim (value (select panel [:#estate-loc-field]))))
		  get-note      (fn [] (str/trim (value (select panel [:#estate-note-field]))))
		  get-address   (fn [] (str/trim (value (select panel [:#estate-address-field]))))
		  get-start     (fn [] (format "%d-%d-1" (value (select panel [:#estate-start-year]))
		  										 (value (select panel [:#estate-start-month]))))

		  estate-id-ok? (fn [] (if (and (utils/is-estate-id? (get-estate-id))
		  							    (not (db/estate-id-exist? (get-estate-id))))
		  							true
		  							(do (alert "Fel fastighets#") false)))
		  estate-ctor (fn [] (hash-map :estate-id (get-estate-id)
		  							   :location (get-loc)
		  							   :address (get-address)
		  							   :note (get-note)
		  							   :from-to (get-start)))
		  make-estate (fn [e] (if-not (estate-id-ok?)
		  						false
	  							(if (= (s/conform estate-spec (estate-ctor)) :clojure.spec/invalid)
   									(do (alert e (s/explain-str estate-spec (estate-ctor))) false)
   									(do (db/add-estate (estate-ctor)) true))))]

		(listen (select panel [:#ok-button]) :action (fn [e] (if (make-estate e)
	                								   			 (restore-main-frame main-panel))))
		(listen (select panel [:#cancel-button]) :action (fn [e] (restore-main-frame main-panel)))

		panel))

(defn do-config
	[main-panel]
	(let [fnt "ARIAL-BOLD-16"
		panel (forms/forms-panel
              "right:pref,8dlu,50dlu,8dlu,50dlu,8dlu,50dlu,8dlu,50dlu"
              :default-row-spec (com.jgoodies.forms.layout.RowSpec. "20px")
              :leading-column-offset 0
              :default-dialog-border? true
              :line-gap-size (com.jgoodies.forms.layout.Sizes/pixel 10)
              :items   [(forms/title "Skapa ny konfiguration") (forms/next-line)
              			"" "Belopp SEK" "Moms %" "Startår"  "Startmånad" (forms/next-line)
                      	"Medlemsavgift"
                      		(spinner :id :membership-fee :font fnt
		 							:model (spinner-model 500 :from 0 :to 1000 :by 1.0))
                      		(spinner :id :membership-fee-tax :font fnt
		 							:model (spinner-model 25.0 :from 0.0 :to 99.9 :by 0.1) :tip "0.0 - 99.9")
		 					(spinner :id :membership-fee-start-year :font fnt
		 							:model (spinner-model (long (utils/year)) :from 2010 :to 2020 :by 1))
		 					(spinner :id :membership-fee-start-month :font fnt
		 							:model (spinner-model (long (utils/month)) :from 1 :to 12 :by 1))
		 					(forms/next-line)
                      	"Anslutningsavgift"
                      		(spinner :id :connection-fee :font fnt
		 							:model (spinner-model 500 :from 0 :to 1000 :by 1.0))
                      		(spinner :id :connection-fee-tax :font fnt
		 							:model (spinner-model 25.0 :from 0.0 :to 99.9 :by 0.1) :tip "0.0 - 99.9")
		 					(spinner :id :connection-fee-start-year :font fnt
		 							:model (spinner-model (long (utils/year)) :from 2010 :to 2020 :by 1))
		 					(spinner :id :connection-fee-start-month :font fnt
		 							:model (spinner-model (long (utils/month)) :from 1 :to 12 :by 1))
		 					(forms/next-line)
                      	"Operatörsavgift"
                      		(spinner :id :operator-fee :font fnt
		 							:model (spinner-model 500 :from 0 :to 1000 :by 1.0))
                      		(spinner :id :operator-fee-tax :font fnt
		 							:model (spinner-model 25.0 :from 0.0 :to 99.9 :by 0.1) :tip "0.0 - 99.9")
		 					(spinner :id :operator-fee-start-year :font fnt
		 							:model (spinner-model (long (utils/year)) :from 2010 :to 2020 :by 1))
		 					(spinner :id :operator-fee-start-month :font fnt
		 							:model (spinner-model (long (utils/month)) :from 1 :to 12 :by 1))
		 					(forms/next-line)
                      	(forms/separator)
                      	(forms/span (button :id :ok-button :text "OK") 1)
                      	(forms/span (button :id :cancel-button :text "Cancel") 1)])

		  get-membership-fee (fn [] {:fee (value (select panel [:#membership-fee]))
		  							 :tax (/ (value (select panel [:#membership-fee-tax])) 100.0)
		  							 :start-year (value (select panel [:#membership-fee-start-year]))
		  							 :start-month (value (select panel [:#membership-fee-start-month]))})
		  get-connection-fee (fn [] {:fee (value (select panel [:#connection-fee]))
		  						     :tax (/ (value (select panel [:#connection-fee-tax])) 100.0)
		  						     :start-year (value (select panel [:#connection-fee-start-year]))
		  						     :start-month (value (select panel [:#connection-fee-start-month]))})
		  get-operator-fee (fn [] {:fee (value (select panel [:#operator-fee]))
		  						   :tax (/ (value (select panel [:#operator-fee-tax])) 100.0)
		  						   :start-year (value (select panel [:#operator-fee-start-year]))
		  						   :start-month (value (select panel [:#operator-fee-start-month]))})
		  make-config (fn [e] (let [fees {:entered    (utils/now-str)
		  								  :membership (get-membership-fee)
		  								  :connection (get-connection-fee)
		  								  :operator   (get-operator-fee)}]
		  						(if (= (s/conform config-spec fees) :clojure.spec/invalid)
    								(do (alert e (s/explain-str config-spec fees)) false)
    								(do (db/add-config fees) true))))]

		(when-let [current (db/get-latest-config)]
			(selection! (select panel [:#membership-fee])     (->> current :membership :fee))
			(selection! (select panel [:#membership-fee-tax]) (->> current :membership :tax))
			(selection! (select panel [:#connection-fee])     (->> current :connection :fee))
			(selection! (select panel [:#connection-fee-tax]) (->> current :connection :tax))
			(selection! (select panel [:#operator-fee])       (->> current :operator :fee))
			(selection! (select panel [:#operator-fee-tax])   (->> current :operator :tax))
			)
		(listen (select panel [:#ok-button]) :action (fn [e]
			(if (make-config e)
				(restore-main-frame main-panel))))
		(listen (select panel [:#cancel-button]) :action (fn [e]
			(restore-main-frame main-panel)))
		panel))

;;------------------------------------------------------------------------------------

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
		  entries (fn [] (map activity-entry (db/get-members-with-estates)))]

		(vertical-panel :items [
						(scrollable (vertical-panel :items (entries)))
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
						[:fill-v 20]])))

;;------------------------------------------------------------------------------------

(defn exec-membership
	[year]
	{:pre [(int? year) (> year 2010) (< year 2021)]}
	(let [fee (fn [member] {:date (utils/today-str)
		  	   				:amount (:tot (:membership (get (db/get-config year data/every-month) 1)))
		  	   				:type :membership-fee
		       				:member-id (:member-id member)
		       				:year year
		  	   				:months data/every-month})]
		(db/set-persist false)
		(doseq [entry (map #(assoc % :debit-credit (conj (:debit-credit %) (fee %)))
						   (db/get-members-not-charged-membership year))]
			(db/add-member entry))
		(db/set-persist true)))

(defn exec-operator
	[year q-months yearly]
	{:pre [(int? year) (> year 2010) (< year 2021) (seq q-months)]}
	(let [config (db/get-config year q-months)
		  members (fn [estate] (db/get-owners-from-estate year estate))
		  billed (fn [estate member-id]
		  	(apply set/union (map :months (filter #(and (= (:year %) year)
					  								    (= (:type %) :operator-fee)
									  				    (seq (set/intersection (:months %) q-months))
									  				    (= (:member-id %) member-id)
									  				    (neg? (:amount %)))
		  										 (:debit-credit estate)))))
		  months (fn [estate member-id] (set/difference q-months (set/intersection (data/get-activity estate year) q-months (billed estate member-id))))
		  fee (fn [estate member-id] {:date (utils/today-str)
		  			                  :amount (reduce + (map #(:tot (:connection (get config %))) (months estate member-id)))
					  			      :type :operator-fee
		  			                  :member-id member-id
		    		                  :year year
		  			                  :months (months estate member-id)})
		  mk-dc (fn [estate] (assoc estate :debit-credit (vec (conj (:debit-credit estate)
		  															(map #(fee estate %) (members estate))))))
		  estates (if yearly
		  			(filter #(db/is-yearly? year %) (db/get-estates-not-charged-oper year q-months))
		  			(remove #(db/is-yearly? year %) (db/get-estates-not-charged-oper year q-months)))]

		(db/set-persist false)
		(doseq [estate (map mk-dc estates)]
			(db/add-estate estate))
		(db/set-persist true)))

(defn exec-connection
	[year q-months yearly]
	{:pre [(int? year) (> year 2010) (< year 2021) (seq q-months)]}
	(let [config (db/get-config year q-months)
		  members (fn [estate] (db/get-owners-from-estate year estate))
		  billed (fn [estate member-id]
		  	(apply set/union (map :months (filter #(and (= (:year %) year)
									 				    (= (:type %) :connection-fee)
									  				    (seq (set/intersection (:months %) q-months))
									  				    (= (:member-id %) member-id)
									  				    (neg? (:amount %)))
		  										 (:debit-credit estate)))))
		  months (fn [estate member-id] (set/difference q-months (set/intersection q-months (billed estate member-id))))
		  fee (fn [estate member-id] {:date (utils/today-str)
		  			                  :amount (reduce + (map #(:tot (:connection (get config %))) (months estate member-id)))
		  			                  :type :connection-fee
		  			                  :member-id member-id
		    		                  :year year
		  			                  :months (months estate member-id)})
		  mk-dc (fn [estate] (assoc estate :debit-credit (vec (conj (:debit-credit estate)
		  															(map #(fee estate %) (members estate))))))
		  estates (if yearly
		  			(filter #(db/is-yearly? year %) (db/get-estates-not-charged-conn year q-months))
		  			(remove #(db/is-yearly? year %) (db/get-estates-not-charged-conn year q-months)))]

		(db/set-persist false)
		(doseq [estate (map mk-dc estates)]
			(db/add-estate estate))
		(db/set-persist true)))

;;------------------------------------------------------------------------------------

(defn do-calc
	[main-panel]
	(try
		(let [fnt "ARIAL-BOLD-16"
			  q-group (button-group)
			  c-group (button-group)
			  panel (forms/forms-panel
              "left:pref,40dlu,40dlu,40dlu,40dlu,40dlu,40dlu,40dlu,40dlu"
              :default-row-spec (com.jgoodies.forms.layout.RowSpec. "20px")
              :leading-column-offset 0
              :default-dialog-border? true
              :line-gap-size (com.jgoodies.forms.layout.Sizes/pixel 10)
              :items   [(forms/title "Beräkna avgifter") (forms/next-line)
                      	(label :text "Vilket år?" :font fnt)
                      		(combobox :id :selected-year :font fnt :model (range 2015 2021))
                      		(forms/next-line)
						(forms/separator "Vad skall beräknas?")
						(forms/span (radio :id :calc-member-radio :text "Beräkna medlemsavgifter"
							   :group c-group :font fnt :selected? true) 5) (forms/next-line)
						(forms/span (radio :id :calc-yearly-con-radio :text "Beräkna anslutningsavgifter för helårabetalare"
							   :group c-group :font fnt) 7) (forms/next-line)
						(forms/span (radio :id :calc-yearly-oper-radio :text "Beräkna användningsavgifter för helårabetalare"
							   :group c-group :font fnt) 7) (forms/next-line)
						(forms/span (radio :id :calc-conn-radio   :text "Beräkna anslutningsavgifter"
							   :group c-group :font fnt) 7) (forms/next-line)
						(forms/span (radio :id :calc-usage-radio  :text "Beräkna användningsavgifter"
							   :group c-group :font fnt) 7) (forms/next-line)
						(forms/separator "Vilket kvartal?")
						(radio :id :q-radio-1 :text "Q1" :group q-group :font fnt :enabled? false :selected? true)
						(radio :id :q-radio-2 :text "Q2" :group q-group :font fnt :enabled? false)
						(radio :id :q-radio-3 :text "Q3" :group q-group :font fnt :enabled? false)
						(radio :id :q-radio-4 :text "Q4" :group q-group :font fnt :enabled? false) (forms/next-line)
                      	(forms/separator)
						(button :id :calc-btn :font fnt :text "Beräkna")
					 	(forms/span (button :id :exec-btn :font fnt :text "Verkställ" :enabled? false) 2) (forms/next-line)
				 		(forms/span (label :id :info-text :halign :center :text "" :font fnt) 7) (forms/next-line)
					 	(forms/separator)
						(button :id :close-button :text "Stäng" :font fnt)])

			  mk-select (fn [s] (select panel [s]))
			  mk-select-i (fn [s i] (select panel (mk-idx-tag s i)))
			  get-year (fn [] (selection (mk-select :#selected-year)))
			  q-radio-on (fn [x] (doseq [i (range 1 5)]
			  						(config! (mk-select-i  "q-radio-" i) :enabled? x)))
			  exec-btn-on (fn [x] (config! (mk-select :#exec-btn) :enabled? x))
			  q-radio-selected! (fn [i x] (config! (mk-select-i "q-radio-" i) :selected? x))
			  q-radio-selected? (fn [i] (config (mk-select-i "q-radio-" i) :selected?))
			  set-info (fn [s] (config! (mk-select :#info-text) :text s))
			  get-q-months (fn [] (cond
			  						(q-radio-selected? 1) #{1 2 3}
			  						(q-radio-selected? 2) #{4 5 6}
			  						(q-radio-selected? 3) #{7 8 9}
			  						(q-radio-selected? 4) #{10 11 12}))
			  calc-action (fn [db-cnt s1 s2] (if (zero? (count db-cnt))
												(set-info (str s1 " är redan gjorda för det valda året"))
												(do
													(set-info (str "Antal " s2 " som blir debiterade: " (count db-cnt)
														           " Vill du verkställa?"))
													(exec-btn-on true))))]

			(listen (select panel [:#calc-member-radio]) :action (fn [e]
				(exec-btn-on false)
				(q-radio-on false)))
			
			(listen (select panel [:#calc-yearly-con-radio]) :action (fn [e]
				(exec-btn-on false)
				(q-radio-on false)))
			
			(listen (select panel [:#calc-yearly-oper-radio]) :action (fn [e]
				(exec-btn-on false)
				(q-radio-on false)))
			
			(listen (select panel [:#calc-conn-radio]) :action (fn [e]
				(exec-btn-on false)
				(q-radio-on true)))
			
			(listen (select panel [:#calc-usage-radio]) :action (fn [e]
				(exec-btn-on false)
				(q-radio-on true)))
			
			(listen (select panel [:#calc-btn]) :action (fn [e]
				(cond
					(config (mk-select :#calc-member-radio) :selected?)
						(calc-action
							(db/get-members-not-charged-membership (get-year))
							"Medlemsavgifterna"
							"medlemmar")

					(config (mk-select :#calc-yearly-con-radio) :selected?)
						(calc-action
							(db/get-estates-not-charged-yearly-con (get-year))
							"Anslutningsavgifterna"
							"fastigheter")

					(config (mk-select :#calc-yearly-oper-radio) :selected?)
						(calc-action
							(db/get-estates-not-charged-yearly-oper (get-year))
							"Användningsavgifterna"
							"fastigheter")

					(config (mk-select :#calc-conn-radio) :selected?)
						(calc-action
							(db/get-estates-not-charged-conn (get-year) (get-q-months))
							"Användningsavgifterna"
							"fastigheter")

					(config (mk-select :#calc-usage-radio) :selected?)
						(calc-action
							(db/get-estates-not-charged-oper (get-year) (get-q-months))
							"Användningsavgifterna"
							"fastigheter")
					)))
			
			(listen (select panel [:#exec-btn]) :action (fn [e]
				(cond
					(config (mk-select :#calc-member-radio) :selected?)
						(do
							(exec-membership (get-year))
							(set-info "Färdigt")
							(exec-btn-on false))

					(config (mk-select :#calc-yearly-con-radio) :selected?)
						(do
							(exec-connection (get-year) data/every-month true)
							(set-info "Färdigt")
							(exec-btn-on false))

					(config (mk-select :#calc-yearly-oper-radio) :selected?)
						(do
							(exec-operator (get-year) data/every-month true)
							(set-info "Färdigt")
							(exec-btn-on false))

					(config (mk-select :#calc-conn-radio) :selected?)
						(do
							(exec-connection (get-year) (get-q-months) false)
							(set-info "Färdigt")
							(exec-btn-on false))

					(config (mk-select :#calc-usage-radio) :selected?)
						(do
							(exec-operator (get-year) (get-q-months) false)
							(set-info "Färdigt")
							(exec-btn-on false))
					)))
			
			(listen (select panel [:#close-button]) :action (fn [e]
				(restore-main-frame main-panel)))

			panel)
		(catch Exception e
			(do
				(error (Exception. e))
				(alert "Ett fel uppstod, kolla logfilen!")))))

;;------------------------------------------------------------------------------------

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
					(menu-item :id :calc-fees          :text "Beräkna avgifter")
					(menu-item :id :invoice-member     :text "Skapa fakturor (medlemsavgift)")
					(menu-item :id :invoice-usage      :text "Skapa fakturor (användning)")
					(menu-item :id :invoice-usage-year :text "Skapa fakturor (användning-helår)")
					(menu-item :id :config-system      :text "Konfigurera")])
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
					(menu-item :id :add-estate :text "Ny fastighet")
					(menu-item :id :test-forms :text "Ändra fastighet")
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
	
	(listen (select panel [:#add-estate]) :action (fn [e]
		(disable-main-menu panel)
		(config! panel :content (do-new-estate panel))))
	
	(listen (select panel [:#test-forms]) :action (fn [e]
		;(config! panel :content (forms-test panel))
		))
	
	(listen (select panel [:#enter-activities]) :action (fn [e]
		(disable-main-menu panel)
		(config! panel :content (activity-frame panel))))
	
	(listen (select panel [:#calc-fees]) :action (fn [e]
		(disable-main-menu panel)
		(config! panel :content (do-calc panel))))

	(listen (select panel [:#invoice-member]) :action (fn [e]
		(when-let [year (input "Välj år:" :choices (range 2010 2021))]
			(disable-main-menu panel)
			(list-invoice-members year)
			(restore-main-frame panel))))

	(listen (select panel [:#invoice-usage]) :action (fn [e]
		(when-let [year (input "Välj år:" :choices (range 2010 2021))]
			(disable-main-menu panel)
			(list-invoice-usage year false)
			(restore-main-frame panel))))

	(listen (select panel [:#invoice-usage-year]) :action (fn [e]
		(when-let [year (input "Välj år:" :choices (range 2010 2021))]
			(disable-main-menu panel)
			(list-invoice-usage year true)
			(restore-main-frame panel))))

	(listen (select panel [:#member-search]) :action (fn [e]
		(disable-main-menu panel)
		(search-members)
		(restore-main-frame panel)))

	(listen (select panel [:#estate-search]) :action (fn [e]
		(disable-main-menu panel)
		(search-estates)
		(restore-main-frame panel)))
	panel))
