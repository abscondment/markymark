(defproject markymark "1.0.0-SNAPSHOT"
  :description "Markdown for Clojure"
  :dependencies [[org.clojure/clojure "1.0.0"]
                 [org.clojure/clojure-contrib "1.0.0"]]
  :dev-dependencies [[leiningen/lein-swank "1.1.0"]
                     [leiningen-run "0.3"]
                     [autodoc "0.7.0"]]
  :script "src/markymark.clj")