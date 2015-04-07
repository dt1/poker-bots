(defproject bots "1.0.0-SNAPSHOT"
  :description "FIXME: write description"
  :dependencies [[org.clojure/clojure "1.6.0"]]

  :main ^:skip-aot bots.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}}  
  :plugins [[cider/cider-nrepl "0.8.2"]])
