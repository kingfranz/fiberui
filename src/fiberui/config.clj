(ns fiberui.config
  (:require [clojure.set    :refer [superset?]]))

(def window-width 1200)
(def window-height 800)

(def min-year 2010)
(def max-year 2020)
(def year-range (range min-year (inc max-year)))
(def month-range (range 1 13))
