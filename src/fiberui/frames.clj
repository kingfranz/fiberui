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
	(:import  javax.swing.JPanel
    		  com.jgoodies.forms.builder.DefaultFormBuilder
    		  com.jgoodies.forms.layout.FormLayout)
    (:use [seesaw.options :only (bean-option default-option
    							 apply-options ignore-options option-map option-provider)]
    	  [seesaw.util :only (resource)]))

;;------------------------------------------------------------------------------------

(defn mk-idx-tag
	[s idx]
	[(keyword (str "#" s idx))])

(defn mk-tag
	[s idx]
	(keyword (str s idx)))
		  
(defn mk-select-tag
	[id nu]
	(keyword (str "#" id "-" nu)))

(defn mk-year-spin
	[current]
	(spinner-model current :from 2010 :to 2020 :by 1))

(defn get-at
	[idx col]
	(nth col idx))
		  
(defn restore-main-frame
	[main-panel]
	(config! (select main-panel [:JMenuItem]) :enabled? true)
	(config! (select main-panel [:JMenu]) :enabled? true)
	(config! (select main-panel [:JMenuBar]) :enabled? true)
	(config! main-panel :title "Databas för fiberföreningen")
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

(defn ^JPanel my-forms-panel
  "Construct a panel with a FormLayout. The column spec is
  expected to be a FormLayout column spec in string form.

  The items are a list of strings, components or any of the
  combinators. For example:

      :items [\"Login\" (text) (next-line)
              \"Password\" (span (text) 3)]

  Takes the following special properties. They correspond
  to the DefaultFormBuilder option of the same name.

      :default-dialog-border?
      :default-row-spec
      :leading-column-offset
      :line-gap-size
      :paragraph-gap-size

  See http://www.jgoodies.com/freeware/forms/index.html"
  {:seesaw {:class `JPanel}}
  [column-spec row-spec & opts]
  (let [layout  (FormLayout. column-spec row-spec)
        panel   (seesaw.core/construct JPanel)
        builder (DefaultFormBuilder. layout panel)]
    (apply-options layout opts)
    (apply-options builder opts)
    (doto (.getPanel builder)
      (apply-options opts))))

(defn search-dialog
	[data-list columns export-title export-cols title]
	(let [return-value (atom nil)
		  panel (my-forms-panel
            "right:pref,5dlu,right:pref,5dlu,fill:pref:grow"
            "pref,10dlu,fill:pref:grow"
			;:default-row-spec (com.jgoodies.forms.layout.RowSpec. "20px")
            ;:leading-column-offset 0
            ;:default-dialog-border? true
            ;:line-gap-size (com.jgoodies.forms.layout.Sizes/pixel 10)
            :items [
              	(checkbox :id :invert-search :text "Invertera?")
              	(checkbox :id :and-search :text "And?")
              	(text :id :search-text :text "" :font "ARIAL-BOLD-18") (forms/next-line)
              	(forms/separator)
                (forms/span (scrollable	(table :id :data-table :font "ARIAL-12")
							:vscroll :always
							:hscroll :always) 5)])

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
		  	:title title
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
	[title]
	(search-dialog (->> (db/get-all-estates)
					(sort-by :estate-id)
					(map #(hash-map :id       (:estate-id %)
									:location (:location %)
									:address  (:address %)
									:text     (str (:estate-id %) (:location %) (:address %)))))
				[:id :location :address]
				[["Fastigheter" (utils/now-str) ""]
				 ["ID" "Betäckning" "Adress"]]
				[:id :location :address]
				title))

(defn search-members
	[title]
	(search-dialog (->> (db/get-all-members)
					(sort-by :member-id)
					(map #(hash-map :id      (:member-id %)
									:namn    (:name %)
									:kontakt (preferred-contact %)
									:text    (str (:member-id %) (:name %) (preferred-contact %)))))
				[:id :namn :kontakt]
				[["Medlemmar" (utils/now-str) ""]
				 ["ID" "Namn" "Kontakt"]]
				[:id :namn :kontakt]
				title))

(defn list-invoice-members
	[year title]
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
				[:medlemsnr :namn :kontact :belopp]
				title))

(defn list-invoice-usage
	[year yearly title]
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
				[:medlemsnr :namn :fastighet :kontakt :conn-fee :oper-fee :tot-amount]
				title)))

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
              :items   ["Medlemsnr" (spinner :id :member-id-nr :font fnt
		 			 			 :model (spinner-model 100 :from 1 :to 1000 :by 1))
                      		(forms/next-line)
                      	"Namn" (forms/span (text :id :member-name-field) 3) (forms/next-line)
                      	"Notering" (forms/span (text :id :member-note-field) 3) (forms/next-line)
                      	(forms/separator "Medlemskapet börjar")
                      	"År" (spinner :id :member-start-year :font fnt
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
		  estate-clicked (fn [idx]  (let [ret (search-estates "Välj fastighet")]
										(config! (select panel [:#estate-name])
												 :text (if (nil? ret) "" ret))))]

		(listen (select panel [:#estate-btn]) :action (fn [e] (estate-clicked 1)))
		(listen (select panel [:#ok-button]) :action (fn [e] (if (make-member e)
	                								   			 (restore-main-frame main-panel))))
		(listen (select panel [:#cancel-button]) :action (fn [e] (restore-main-frame main-panel)))

		panel))

;:debit-credit  [{:date "2016-12-5"
; 				 :amount -500
; 				 :type :membership-fee
; 				 :year 2016
; 				 :months #{1 2 3 4 5 6 7 8 9 10 11 12}}]

(defn edit-member
	[main-panel member-id]
	(let [contact-types {"Adress" :address "E-Post" :email "Telefon" :phone}
		  dc-types {"Medlemskap" :membership-fee "Anslutning" :connection-fee "Användning" :operator-fee}
		  fnt "ARIAL-BOLD-14"
		  member (db/get-member member-id)
		  get-at (fn [idx col] (nth col idx))
		  get-ym (fn [x k ym] (if-let [ym-ym (f/parse (get (:from-to x) k))]
		  						  (long (ym ym-ym))
		  						  (long (if (= ym t/year) 2020 12))))
		  mk-estate (fn [idx]
		  	(let [estate (->> member :estates (get-at idx))
		  		  has-to? (utils/not-nil? (->> estate :from-to :to))
		  		  yss (spinner-model (->> estate :from-to :from f/parse t/year long)
		  							 :from 2010 :to 2020 :by 1)
		  		  yes (spinner-model (get-ym estate :to t/year)
		  						 	 :from 2010 :to 2020 :by 1)
		  		  mss (spinner-model (->> estate :from-to :from f/parse t/month long)
		  						 	 :from 1 :to 12 :by 1)
		  		  mes (spinner-model (get-ym estate :to t/month)
		  		 					 :from 1 :to 12 :by 1)]

		  		[(text :id (mk-tag "estate-name-" idx) :font fnt :text (:estate-id estate))
				 "Start-År" (spinner :id (mk-tag "estate-start-year-" idx) :font fnt :model yss)
                 "Start-Månad" (spinner :id (mk-tag "estate-start-month-" idx) :font fnt :model mss)
                 (checkbox :id (mk-tag "estate-disable-" idx) :text "Avsluta?")
                 (label :text "Slut-År" :enabled? has-to?)
                 (spinner :id (mk-tag "estate-end-year-" idx)   :font fnt :model yes :enabled? has-to?)
                 (label :text "Slut-Månad" :enabled? has-to?)
                 (spinner :id (mk-tag "estate-end-month-" idx) :font fnt :model mes :enabled? has-to?)
                 (forms/next-line)]))

		  estate-part (forms/forms-panel
              "right:pref,12dlu, right:pref,3dlu, 40dlu,12dlu, right:pref,3dlu, 60dlu,20dlu, right:pref,10dlu, right:pref,3dlu, 40dlu,12dlu, right:pref,3dlu, 30dlu, 80dlu"
              :default-row-spec (com.jgoodies.forms.layout.RowSpec. "20px")
              :leading-column-offset 0
              :default-dialog-border? true
              :line-gap-size (com.jgoodies.forms.layout.Sizes/pixel 10)
              :items (concat [(forms/separator "Fastigheter")]
              				 (mapcat mk-estate (range (count (:estates member))))))

		  dc-part (let [dc (->> member :debit-credit)
		  				mk-date (fn [idx] (text :id (mk-tag "dc-date-" idx) :font fnt :text (:date (get-at idx dc))))
		  				mk-money-spin (fn [idx] (spinner-model (:amount (get-at idx dc)) :from -1000 :to 1000 :by 0.01))
		  				mk-tax-spin (fn [idx] (spinner-model (:tax (get-at idx dc)) :from -1000 :to 1000 :by 0.01))
		  				mk-cb (fn [idx m] (checkbox :id (mk-tag (str "dc-month-" m "-") idx) :text (str m)
		  					:selected? (contains? (:months (get-at idx dc)) m)))
		  				mk-entry (fn [idx]
		  					(concat [(mk-date idx)]
		  	 				 [(combobox :id (mk-tag "dc-type-" idx) :model (vec (keys dc-types)))]
		  	 				 ["Belopp"] [(spinner :id (mk-tag "dc-amount-" idx) :font fnt :model (mk-money-spin idx))]
		  	 				 ["Moms"] [(spinner :id (mk-tag "dc-tax-" idx) :font fnt :model (mk-tax-spin idx))]
		  	 				 ["År"] [(spinner :id (mk-tag "dc-year-" idx) :font fnt :model (mk-year-spin (:year (get-at idx dc))))]
		  	 				 (map #(mk-cb idx %) (range 1 13))))]
		  				;(println "dc-part1:" (mk-cb 0 1))
		  	(forms/forms-panel
              "right:pref,5dlu, pref,8dlu, right:pref,5dlu, right:pref,5dlu, pref,8dlu, right:pref,5dlu, pref,10dlu, 15dlu,3dlu, 15dlu,3dlu, 15dlu,3dlu, 15dlu,3dlu, 15dlu,3dlu, 15dlu,3dlu, 15dlu,3dlu, 15dlu,3dlu, 15dlu,3dlu, 20dlu,3dlu, 20dlu,3dlu, 20dlu, 30dlu"
              :default-row-spec (com.jgoodies.forms.layout.RowSpec. "20px")
              :leading-column-offset 0
              :default-dialog-border? true
              :line-gap-size (com.jgoodies.forms.layout.Sizes/pixel 10)
              :items (concat [(forms/separator "Debit/Credit")]
              	(mapcat mk-entry (range (count dc))))))

		  info-part (forms/forms-panel
              "right:pref,8dlu,30dlu,150dlu"
              :default-row-spec (com.jgoodies.forms.layout.RowSpec. "20px")
              :leading-column-offset 0
              :default-dialog-border? true
              :line-gap-size (com.jgoodies.forms.layout.Sizes/pixel 10)
              :items [
              	 (forms/separator "Medlem")
                 "Medlemsnr" (spinner :id :member-id-nr :font fnt
		 			 				  :model (spinner-model member-id :from 1 :to 1000 :by 1))
                 (forms/next-line)
                 "Namn" (forms/span (text :id :member-name-field :text (:name member)) 2) (forms/next-line)
                 "Notering" (forms/span (text :id :member-note-field :text (:note member)) 2)])

		  start-part (forms/forms-panel
              "right:pref,8dlu, 50dlu,8dlu, right:pref,8dlu, 50dlu,50dlu"
              :default-row-spec (com.jgoodies.forms.layout.RowSpec. "20px")
              :leading-column-offset 0
              :default-dialog-border? true
              :line-gap-size (com.jgoodies.forms.layout.Sizes/pixel 10)
              :items [
              	 (forms/separator "Medlemskapet börjar --- och slutar")
                 "" "" (checkbox :id :member-disable :text "Avsluta?" :halign :left)
                 (forms/next-line)
                 "År" (spinner :id :member-start-year :font fnt
		 		 			 :model (spinner-model (->> member :from-to :from f/parse t/year long)
		 		 			 	:from 2010 :to 2020 :by 1))
                 (label :text "År" :enabled? (utils/not-nil? (->> member :from-to :to)))
                 (spinner :id :member-end-year :font fnt
		 		 			   :model (spinner-model (get-ym member :to t/year):from 2010 :to 2020 :by 1)
		 		 			   :enabled? (utils/not-nil? (->> member :from-to :to)))
                 (forms/next-line)
                 "Månad" (spinner :id :member-start-month :font fnt
		 			 			  :model (spinner-model (->> member :from-to :from f/parse t/month long) :from 1 :to 12 :by 1))
                 (label :text "Månad" :enabled? (utils/not-nil? (->> member :from-to :to)))
                 (spinner :id :member-end-month :font fnt
		 			 			  :model (spinner-model (get-ym member :to t/month) :from 1 :to 12 :by 1)
		 		 			      :enabled? (utils/not-nil? (->> member :from-to :to)))])

		  contact-part (let [
		  	contacts (->> member :contact)
		  	num-contact-fields (max (inc (int (/ (count contacts) 2))) 4)
		  	get-pref (->> contacts (filter :preferred) first)
		  	get-skipped (fn [idx] (->> contacts (remove :preferred) (sort-by :type) (get-at (dec idx))))
		  	get-type-str (fn [cont] (get (set/map-invert contact-types) (:type cont)))
		  	get-contact-type (fn [idx]  (if (>= idx (count contacts))
		  								    "Adress"
		  								    (if (= idx 0)
		  								   		(get-type-str get-pref)
		  								   		(get-type-str (get-skipped idx)))))
		  	get-contact-text (fn [idx]  (if (>= idx (count contacts))
		  								    ""
		  								    (if (= idx 0)
		  								   		(:value get-pref)
		  								   		(:value (get-skipped idx)))))
			mk-contact (fn [idx]
				(concat [(combobox :id (mk-tag "member-contact-type-" idx)
								   :model (vec (keys contact-types))
								   :selected-item (get-contact-type idx))]
 						[(text :id (mk-tag "member-contact-field-" idx)
 							   :text (get-contact-text idx))]))]
		  	(forms/forms-panel
            	"right:pref,8dlu,200dlu,8dlu,right:pref,8dlu,200dlu"
            	:default-row-spec (com.jgoodies.forms.layout.RowSpec. "20px")
            	:leading-column-offset 0
            	:default-dialog-border? true
            	:line-gap-size (com.jgoodies.forms.layout.Sizes/pixel 10)
            	:items (concat 
            		[(forms/separator "Kontakter (den första blir faktureringsadress)")]
              		(mapcat mk-contact (range num-contact-fields)))))

		  panel (my-forms-panel
                "left:pref"
                "pref, 90dlu, 90dlu, 90dlu:grow, 20dlu, center:pref"
                :default-dialog-border? true
            	:line-gap-size (com.jgoodies.forms.layout.Sizes/pixel 0)
            	:items [
			  		(left-right-split info-part	start-part :divider-location 1/2 :border 0)
					(scrollable estate-part :hscroll :never :border 0)
					(scrollable contact-part :hscroll :never :border 0)
					(scrollable dc-part :hscroll :never :border 0)
		            (forms/separator)
		            (forms/forms-panel
	            		"pref,30dlu,pref"
	            		:leading-column-offset 100
	            		:items [
							(button :id :ok-button :text "OK")
							(button :id :cancel-button :text "Cancel")])])

                      	

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
		  estate-clicked (fn [idx]  (let [ret (search-estates "Välj fastighet")]
										(config! (select panel [:#estate-name])
												 :text (if (nil? ret) "" ret))))]

		;(listen (select panel [:#estate-btn]) :action (fn [e] (estate-clicked 1)))
		(listen (select panel [:#ok-button]) :action (fn [e] (if (make-member e)
	                								   			 (restore-main-frame main-panel))))
		(listen (select panel [:#cancel-button]) :action (fn [e] (restore-main-frame main-panel)))

		panel))

(def ^:private estate-example
	{:estate-id        "MBEF82"
	 :location         "Myrhult 1:2"
	 :address          "Lindåsen Höjen 52, 54592 Älgarås"
	 :note             ""
	 :from-to          {:from "2016-12-5" :to "2016-12-5"}
	 :billing-interval [{:year 2016 :months 12} {:year 2017 :months 3}]
	 :activity         [{:year 2016 :months #{1 2 3 10 11 12}}]
	 :debit-credit     [{:date "2016-12-5"
	 					 :amount 40
	 					 :tax 10
	 					 :type :connection-fee
	 					 :member-id 3
	 					 :year 2016
	 					 :months #{1 2 3}}]
	 })

(defn mk-month-boxes
	[month-txt fnt row-num key-txt value-set]
	(let [mk-cb (fn [m]
					(checkbox :id (mk-tag (str key-txt row-num "-") m)
			  				  :text (nth month-txt (dec m))
			  				  :font fnt
			  				  :selected? (contains? value-set m)))]
	;(println "key:" key-txt "values:" value-set)
		(grid-panel :columns 12 :items [
			(mk-cb 1) (mk-cb 2) (mk-cb 3) (mk-cb 4)
			(mk-cb 5) (mk-cb 6) (mk-cb 7) (mk-cb 8)
			(mk-cb 9) (mk-cb 10) (mk-cb 11) (mk-cb 12)])))

(defn edit-estate
	[main-panel estate-id]
	(let [dc-types {"Medlemskap" :membership-fee "Anslutning" :connection-fee "Användning" :operator-fee}
		  fnt "ARIAL-BOLD-14"
		  estate (db/get-estate estate-id)
		  get-ym (fn [x k ym] (if-let [ym-ym (f/parse (get (:from-to x) k))]
		  						  (long (ym ym-ym))
		  						  (long (if (= ym t/year) 2020 12))))
		  dc-part (let [dc (->> estate :debit-credit)
		  				mk-date (fn [idx] (text :id (mk-tag "dc-date-" idx) :margin 3 :font fnt :text (:date (get-at idx dc))))
		  				mk-money-spin (fn [idx] (spinner-model (:amount (get-at idx dc)) :from -1000 :to 1000 :by 0.01))
		  				mk-tax-spin (fn [idx] (spinner-model (:tax (get-at idx dc)) :from -1000 :to 1000 :by 0.01))
		  				mk-entry (fn [idx]
		  					[(mk-date idx)
		  	 				 (selection! (combobox :id (mk-tag "dc-type-" idx)
		  	 				 					   :model (vec (keys dc-types)))
		  	 				 			 (get (set/map-invert dc-types) (:type (get-at idx dc))))
		  	 				 (spinner :id (mk-tag "dc-amount-" idx) :font fnt :model (mk-money-spin idx))
		  	 				 (spinner :id (mk-tag "dc-tax-" idx) :font fnt :model (mk-tax-spin idx))
		  	 				 (spinner :id (mk-tag "dc-year-" idx) :font fnt :model (mk-year-spin (:year (get-at idx dc))))
		  	 				 (mk-month-boxes ["J" "F" "M" "A" "M" "J"
			  								  "J" "A" "S" "O" "N" "D"]
			  								  fnt idx "dc-month-" (:months (get-at idx dc)))
		  	 				 (checkbox :id (mk-tag "dc-del-" idx) :font fnt)])]
		  	(vertical-panel :items [
		  		(forms/forms-panel
	              	"center:50dlu,5dlu, center:50dlu,5dlu, center:30dlu,5dlu, center:30dlu,5dlu, center:30dlu,5dlu, center:230dlu,10dlu, 10dlu"
	              	:default-row-spec (com.jgoodies.forms.layout.RowSpec. "20px")
	              	:leading-column-offset 0
	              	:default-dialog-border? true
	              	:line-gap-size (com.jgoodies.forms.layout.Sizes/pixel 10)
	              	:items [
	              		(forms/separator "Debit/Credit")
	              		"Införd" "Type" "Belopp" "Moms" "År" "Månader" "X"])
	            (scrollable (forms/forms-panel
	              	"center:50dlu,5dlu, center:50dlu,5dlu, center:30dlu,5dlu, center:30dlu,5dlu, center:30dlu,5dlu, center:230dlu,10dlu, 10dlu"
	              	:items (mapcat mk-entry (range (count dc))))
	            	:hscroll :never :vscroll :always :border 0)
	            (button :text "Lägg till ny")]))

		  info-part (forms/forms-panel
              "right:pref,8dlu, right:pref,8dlu, right:pref,8dlu, right:pref,8dlu, right:pref,8dlu, right:pref,8dlu, right:pref,8dlu, right:pref,8dlu"
              :default-row-spec (com.jgoodies.forms.layout.RowSpec. "20px")
              :leading-column-offset 0
              :default-dialog-border? true
              :line-gap-size (com.jgoodies.forms.layout.Sizes/pixel 10)
              :items [
              	 "ID" (text :id :estate-id-field :text (:estate-id estate))
                 "Betäckning" (text :id :estate-loc-field :text (:location estate))
                 "Ägare" (text :text (:name (db/get-current-owner-from-estate estate)))
                 (forms/next-line)
                 "Adress" (forms/span (text :id :estate-address-field :text (:address estate)) 5)
                 "Notering" (forms/span (text :id :estate-note-field :text (:note estate)) 5)])

		  start-part (forms/forms-panel
              "right:pref,8dlu, 50dlu,30dlu, right:pref,8dlu, right:pref,8dlu, 50dlu,10dlu"
              :default-row-spec (com.jgoodies.forms.layout.RowSpec. "20px")
              :leading-column-offset 0
              :default-dialog-border? true
              :line-gap-size (com.jgoodies.forms.layout.Sizes/pixel 10)
              :items [
              	 (forms/separator "Anslutningen börjar --- och slutar")
                 (forms/next-line)
                 "År" (spinner :id :estate-start-year :font fnt
		 		 			 :model (spinner-model (->> estate :from-to :from f/parse t/year long)
		 		 			 	:from 2010 :to 2020 :by 1))
                 (checkbox :id :estate-disable :text "Avsluta?")
                 (label :text "År" :enabled? (utils/not-nil? (->> estate :from-to :to)))
                 (spinner :id :estate-end-year :font fnt
		 		 			   :model (spinner-model (get-ym estate :to t/year):from 2010 :to 2020 :by 1)
		 		 			   :enabled? (utils/not-nil? (->> estate :from-to :to)))
                 "Månad" (spinner :id :estate-start-month :font fnt
		 			 			  :model (spinner-model (->> estate :from-to :from f/parse t/month long) :from 1 :to 12 :by 1))
                 "" (label :text "Månad" :enabled? (utils/not-nil? (->> estate :from-to :to)))
                 (spinner :id :estate-end-month :font fnt
		 			 			  :model (spinner-model (get-ym estate :to t/month) :from 1 :to 12 :by 1)
		 		 			      :enabled? (utils/not-nil? (->> estate :from-to :to)))])

		  interval-part
		  	(let [bis (->> estate :billing-interval (sort-by :year))
		  		  bi-group (button-group)
		  		  mk-bi-entry (fn [idx]
		  			[(spinner :id (mk-tag "bi-year-" idx)
		  					  :font fnt
		  					  :model (mk-year-spin (:year (get-at idx bis))))
		  	 		 (selection! (combobox :id (mk-tag "bi-months-" idx)
		  	 							   :model [3 12])
		  	 					 (:months (get-at idx bis)))])]
			  	(vertical-panel :items [
			  		(forms/forms-panel
	            		"center:50dlu,15dlu, center:50dlu"
	            		:default-row-spec (com.jgoodies.forms.layout.RowSpec. "20px")
		                :leading-column-offset 0
		                :default-dialog-border? true
		                :line-gap-size (com.jgoodies.forms.layout.Sizes/pixel 10)
		                :items [(forms/separator "Betalningsinterval")])
	          		(scrollable (forms/forms-panel
	          						"center:50dlu,15dlu, center:50dlu"
	          						:items (mapcat mk-bi-entry (range (count bis))))
	          			:hscroll :never :vscroll :always :border 0)]))

		  activity-part
		  	(let [acts (->> estate :activity (sort-by :year))
		  		  mk-act-entry (fn [idx]
		  			[(spinner :id (mk-tag "act-year-" idx)
		  					  :font fnt
		  					  :model (mk-year-spin (:year (get-at idx acts))))
		  	 		 (mk-month-boxes ["Jan" "Feb" "Mar" "Apr" "Maj" "Jun"
			  						  "Jul" "Aug" "Sep" "Okt" "Nov" "Dec"]
			  						 fnt idx "act-" (:months (get-at idx acts)))
		  	 		 (checkbox :id (mk-tag "act-del-" idx) :font fnt)])]
		  	(vertical-panel :items [
			  	(forms/forms-panel
              		"center:50dlu,15dlu, left:300dlu,10dlu, 10dlu"
              		:default-row-spec (com.jgoodies.forms.layout.RowSpec. "20px")
              		:leading-column-offset 0
              		:default-dialog-border? true
              		:line-gap-size (com.jgoodies.forms.layout.Sizes/pixel 10)
              		:items [(forms/separator "Aktiviteter")])
              	(scrollable (forms/forms-panel
	          		"center:50dlu,15dlu, left:300dlu,10dlu, 10dlu"
	          		:items (mapcat mk-act-entry (range (count acts))))
	          		:hscroll :never :vscroll :always :border 0)
              	(button :text "Lägg till ny")]))

		  panel (my-forms-panel
                "left:pref"
                "pref, top:100dlu, top:100dlu, top:pref:grow, center:pref"
                :default-dialog-border? true
            	:line-gap-size (com.jgoodies.forms.layout.Sizes/pixel 0)
            	:items [
			  		info-part
			  		(left-right-split start-part interval-part)
					activity-part
					dc-part
		            (forms/separator)
		            (forms/forms-panel
	            		"pref,30dlu,pref"
	            		:leading-column-offset 100
	            		:items [
							(button :id :ok-button :text "OK")
							(button :id :cancel-button :text "Cancel")])])]

                      	
;		(listen (select panel [:#ok-button]) :action (fn [e] (if (make-member e)
;	                								   			 (restore-main-frame main-panel))))
;		(listen (select panel [:#cancel-button]) :action (fn [e] (restore-main-frame main-panel)))

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
              :items   ["Projektnummer" (forms/span (text :id :estate-id-field) 3) (forms/next-line)
                      	"Location" (forms/span (text :id :estate-loc-field) 3) (forms/next-line)
                      	"Adress" (forms/span (text :id :estate-address-field) 3) (forms/next-line)
                      	"Note" (forms/span (text :id :estate-note-field) 3) (forms/next-line)
                      	(forms/separator "Anslutningen börjar")
                      	"År" (spinner :id :estate-start-year :font fnt
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

;		  a {:member-id
;		     :name
;		     :membership {:fee :tax}
;		     :estates [{:estate-id :connection {:fee :tax} :operator {:fee :tax}}]}
(defn enter-fees
	[main-panel]
	(let [fnt "ARIAL-BOLD-14"
		  owe-entry (fn [member]
			(let [mk-membership-line [
			  		"" "Medlemsavgift"
	        		"Belopp:" (label :text (str (:fee (:membership member))))
	        		"Moms:" (label :text (str (:tax (:membership member))))
	        		"Totalt:" (label :text (str (+ (:fee (:membership member)) (:tax (:membership member)))))
	        		"När:" (text :id (mk-tag "date-paid-" (:member-id member)) :text (utils/today-str))
	        		(checkbox :id (mk-tag "paid-all-" (:member-id member)) :text "Betalat allt?")
	        		"Betalt:" (spinner :id (mk-tag "paid-amount-" (:member-id member)) :font fnt
	 			 			  		   :model (spinner-model 1 :from 1 :to 10000 :by 1))]
	              mk-conn-line (fn [estate] [
			  		(label :text (:estate-id estate)) "Anslutningsavgift"
	        		"Belopp:" (label :text (str (:fee (:connection estate))))
	        		"Moms:" (label :text (str (:tax (:connection estate))))
	        		"Totalt:" (label :text (str (+ (:fee (:connection estate)) (:tax (:connection estate)))))
	        		"När:" (text :id (mk-tag "conn-date-paid-" (str (:member-id member) "-" (:estate-id estate))) :text (utils/today-str))
	        		(checkbox :id (mk-tag "conn-paid-all-" (str (:member-id member) "-" (:estate-id estate))) :text "Betalat allt?")
	        		"Betalt:" (spinner :id (mk-tag "conn-paid-amount-" (str (:member-id member) "-" (:estate-id estate))) :font fnt
	 			 			  :model (spinner-model 1 :from 1 :to 10000 :by 1))])
	              mk-oper-line (fn [estate] [
			  		(label :text (:estate-id estate)) "Användningsavgift"
	        		"Belopp:" (label :text (str (:fee (:operator estate))))
	        		"Moms:" (label :text (str (:tax (:operator estate))))
	        		"Totalt:" (label :text (str (+ (:fee (:operator estate)) (:tax (:operator estate)))))
	        		"När:" (text :id (mk-tag "oper-date-paid-" (str (:member-id member) "-" (:estate-id estate))) :text (utils/today-str))
	        		(checkbox :id (mk-tag "oper-paid-all-" (str (:member-id member) "-" (:estate-id estate))) :text "Betalat allt?")
	        		"Betalt:" (spinner :id (mk-tag "oper-paid-amount-" (str (:member-id member) "-" (:estate-id estate))) :font fnt
	 			 			  		   :model (spinner-model 1 :from 1 :to 10000 :by 1))])
	              ]
				(forms/forms-panel
            		"right:pref,8dlu,right:pref,8dlu,right:pref,8dlu,right:pref,8dlu,right:pref,8dlu,right:pref,8dlu,right:pref,8dlu,right:pref,8dlu,right:pref,8dlu,right:pref,8dlu,right:pref,8dlu,right:pref,8dlu,right:pref"
	            	:default-row-spec (com.jgoodies.forms.layout.RowSpec. "20px")
	            	:leading-column-offset 0
	            	:default-dialog-border? true
	            	:line-gap-size (com.jgoodies.forms.layout.Sizes/pixel 10)
	            	:items (concat 
	            		[(forms/separator (str (:member-id member) " " (:name member)))]
	            		mk-membership-line
	              		(mapcat mk-conn-line (:estates member))
	              		(mapcat mk-oper-line (:estates member))))))

		  sum-of-dc (fn [m-id t dc] (let [v2m (fn [[fee tax]] {:fee fee :tax tax})
		  								  valu (->> dc (filter #(and (= (:member-id %) m-id) (= (:type %) t)))
		  											   (map #(vector (:amount %) (:tax %)))
		  									           (reduce (fn [[x1 y1] [x2 y2]] [(+ x1 x2) (+ y1 y2)]) [0 0]))]
		  								(v2m valu)))

		  members-who-owe (let [
		  		members   (db/get-all-members)
		  		owe-memb  (fn [member] {:member-id (:member-id member)
		  								:name (:name member)
		  								:membership (sum-of-dc (:member-id member) :membership-fee (:debit-credit member))})
		  		owe-conn (fn [estate member-id] (sum-of-dc member-id :connection-fee (:debit-credit estate)))
		  		owe-oper (fn [estate member-id] (sum-of-dc member-id :operator-fee   (:debit-credit estate)))
		  		owe-estates (fn [member] (map #(hash-map :estate-id (:estate-id %)
		  												 :connection (owe-conn % (:member-id member))
		  												 :operator (owe-oper % (:member-id member))) (:estates member)))
		  		aaa (map #(assoc (owe-memb %) :estates (owe-estates %)) members)
		  		sum-estates (fn [estates] (reduce + (map #(+ (:fee (:connection %)) (:tax (:connection %))
		  										             (:fee (:operator %)) (:tax (:operator %))) estates)))
		  		f-fn (fn [memb] (< (+ (:fee (:membership memb)) (:tax (:membership memb)) (sum-estates (:estates memb))) 0))]
		  	(sort-by :member-id (filter f-fn aaa)))

		  entries (fn [] (map owe-entry members-who-owe))]

		(vertical-panel :items [
						(scrollable (vertical-panel :items (entries)))
						:separator
						[:fill-v 20]
						(horizontal-panel :items [
							(button :text "OK"
									:listen [:action (fn [e]
										;(update-entries)
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
		  	   				:amount (:fee (:membership (get (db/get-config year data/every-month) 1)))
		  	   				:tax (:tax (:membership (get (db/get-config year data/every-month) 1)))
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
		  			                  :amount (reduce + (map #(:fee (:connection (get config %))) (months estate member-id)))
					  			      :tax (reduce + (map #(:tax (:connection (get config %))) (months estate member-id)))
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
		  			                  :amount (reduce + (map #(:fee (:connection (get config %))) (months estate member-id)))
		  			                  :tax (reduce + (map #(:tax (:connection (get config %))) (months estate member-id)))
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
		:title "Databas för fiberföreningen"
		:on-close :exit
		:width window-width
		:height window-height
		:resizable? false
		:menubar (menubar :id :menu-bar :items [
			(menu
				:id :system-menu
				:text "System"
				:items [
					(menu-item :id :calc-fees          :text "Beräkna avgifter")
					(menu-item :id :invoice-member     :text "Skapa fakturor (medlemsavgift)")
					(menu-item :id :invoice-usage      :text "Skapa fakturor (användning)")
					(menu-item :id :invoice-usage-year :text "Skapa fakturor (användning-helår)")
					(menu-item :id :enter-fees         :text "Bokför inbetalningar")
					(menu-item :id :config-system      :text "Konfigurera")])
			(menu
				:id :member-menu
				:text "Medlemmar"
				:items [
					(menu-item :id :add-member :text "Ny medlem")
					(menu-item :id :edit-member :text "Ändra medlem")
					(menu-item :id :member-search :text "Sök medlemmar")])
			(menu
				:id :estate-menu
				:text "Fastigheter"
				:items [
					(menu-item :id :add-estate :text "Ny fastighet")
					(menu-item :id :edit-estate :text "Ändra fastighet")
					(menu-item :id :estate-search :text "Sök fastighet")
					(menu-item :id :enter-activities :text "Bokför aktiviteter")])]))]

	(listen (select panel [:#calc-fees]) :action (fn [e]
		(disable-main-menu panel)
		(config! panel :title "Beräkna avgifter")
		(config! panel :content (do-calc panel))))

	(listen (select panel [:#invoice-member]) :action (fn [e]
		(when-let [year (input "Välj år:" :choices (range 2010 2021))]
			(disable-main-menu panel)
			(config! panel :title "Medlemsavgifter")
			(list-invoice-members year "Medlemsavgifter")
			(restore-main-frame panel))))

	(listen (select panel [:#invoice-usage]) :action (fn [e]
		(when-let [year (input "Välj år:" :choices (range 2010 2021))]
			(disable-main-menu panel)
			(config! panel :title "Avgifter för användning")
			(list-invoice-usage year false "Avgifter för användning")
			(restore-main-frame panel))))

	(listen (select panel [:#invoice-usage-year]) :action (fn [e]
		(when-let [year (input "Välj år:" :choices (range 2010 2021))]
			(disable-main-menu panel)
			(config! panel :title "Avgifter för användning (helår)")
			(list-invoice-usage year true "Avgifter för användning (helår)")
			(restore-main-frame panel))))

	(listen (select panel [:#enter-fees]) :action (fn [e]
		(disable-main-menu panel)
		(config! panel :title "Inbetalningar")
		(config! panel :content (enter-fees panel))))
	
	(listen (select panel [:#config-system]) :action (fn [e]
		(disable-main-menu panel)
		(config! panel :title "Konfigurera")
		(config! panel :content (do-config panel))))
	
	(listen (select panel [:#add-member]) :action (fn [e]
		(disable-main-menu panel)
		(config! panel :title "Lägg till medlem")
		(config! panel :content (do-new-member panel))))
	
	(listen (select panel [:#edit-member]) :action (fn [e]
		;(when-let [member-id (search-members "Välj medlem")]
			(disable-main-menu panel)
			(config! panel :title "Ändra medlem")
			;(config! panel :content (edit-member panel member-id)))))
			(config! panel :content (edit-member panel 5))))
	
	(listen (select panel [:#member-search]) :action (fn [e]
		(disable-main-menu panel)
		(config! panel :title "Sök medlemmar")
		(search-members "Medlemmar")
		(restore-main-frame panel)))

	(listen (select panel [:#add-estate]) :action (fn [e]
		(disable-main-menu panel)
		(config! panel :title "Lägg till fastighet")
		(config! panel :content (do-new-estate panel))))
	
	(listen (select panel [:#edit-estate]) :action (fn [e]
		(when-let [estate-id (search-estates "Välj fastighet")]
			(disable-main-menu panel)
			(config! panel :title "Ändra fastighet")
			(config! panel :content (edit-estate panel estate-id)))))
	
	(listen (select panel [:#estate-search]) :action (fn [e]
		(disable-main-menu panel)
		(config! panel :title "Sök fastigheter")
		(search-estates "Fastigheter")
		(restore-main-frame panel)))
	
	(listen (select panel [:#enter-activities]) :action (fn [e]
		(disable-main-menu panel)
		(config! panel :title "Bokför aktiviteter")
		(config! panel :content (activity-frame panel))))
	panel))
