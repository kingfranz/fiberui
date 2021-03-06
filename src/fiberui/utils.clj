(ns fiberui.utils
    (:require [clj-time.core              :as t])
    (:require [clj-time.format            :as f])
    (:require [clj-time.local             :as l])
    (:require [clojure.string :as str])
    (:require [clojure.string :refer [trim]]))

(defn must-string
    [name value]
    (if (nil? value)
        (throw (Exception. (str name " is nil")))
        (if-not (string? value)
            (throw (Exception. (str name " is not a string: " value)))
            (if (= (count value) 0)
                (throw (Exception. (str name " is empty")))
                value))))

(defn pr1
	[s e]
	(println s e)
	e)

(defn set-var 
    "set value of atom"
    [the-atom value]
    (swap! the-atom (fn [x] value))
    value)

(defn not-nil?
    [params]
    (not (nil? params)))

(defn not-blank?
    [s]
    (not (str/blank? s)))

(defn is-string?
    [s]
    (and (not-nil? s) (string? s) (not-blank? s)))

(defn this-year?
    [y s]
    (if-let [d (f/parse s)]
        (= (t/year d) y)
        (throw (Exception. (str "this-year?: " s " is not a date string")))))

(defn parse-int [s]
    (Integer. (re-find  #"\d+" (trim s))))

(defn is-int-str?
    [s]
    (and (is-string? s) (re-matches #"-?\d+" (trim s))))

(defn is-pos-int-str?
    [s]
    (and (is-string? s) (re-matches #"\d+" (trim s))))

(defn valid-email?
    [e]
    (and (is-string? e) (re-matches #"(?i)[^@]+@[A-Z0-9.-]+\.[A-Z]{2,4}" e)))
	
(defn valid-phone?
    [e]
    (and (is-string? e) (re-matches #"[0-9 +-]+" e)))

(defn date?
    [x]
    (and (is-string? x) (f/parse (f/formatters :date) x)))

(defn date-time?
    [x]
    (and (is-string? x) (f/parse (f/formatters :mysql) x)))

(defn is-pos-int? 
    "doc-string"
    [x]
    (and (integer? x) (pos? x)))

; return current dat & time as a string
(defn now-str
    []
    (f/unparse (f/formatters :mysql) (l/local-now)))

(defn year
    []
    (t/year (l/local-now)))

(defn month
    []
    (t/month (l/local-now)))

(defn today-str
    []
    (f/unparse (f/formatters :date) (l/local-now)))

