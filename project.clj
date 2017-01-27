(defproject fiberui "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0-alpha14"]
                 [com.taoensso/timbre "4.7.4"]
                 [org.clojure/math.numeric-tower "0.0.4"]
                 [seesaw "1.4.6-SNAPSHOT"]
                 [com.jgoodies/forms "1.2.1"]
                 [org.clojure/data.csv "0.1.3"]
                 [clj-time "0.12.0"]
                 [gardendb "0.2.0"]]
  :main ^:skip-aot fiberui.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
