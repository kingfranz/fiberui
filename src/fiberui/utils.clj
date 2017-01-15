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

(defn set-var 
    "set value of atom"
    [the-atom value]
    (swap! the-atom (fn [x] value))
    value)

(defn not-nil?
    [params]
    (not (nil? params)))

(defn checked? [c]
    (and c (= c "on")))

(defn is-string?
    [s]
    (and (not-nil? s) (string? s) (> (count (trim s)) 0)))

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
    (and (is-string? e) (re-matches #"(\+[1-9])|(0)[0-9 -]+" e)))

(defn date?
    [x]
    (and (is-string? x) (f/parse x)))

(defn is-estate-id?
    [x]
    (and (is-string? x) (re-matches #"MBEF[0-9]+" x)))

(defn is-pos-int? 
    "doc-string"
    [x]
    (and (int? x) (>= x 0)))

