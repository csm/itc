(defproject com.github.csm/itc "0.1.2-SNAPSHOT"
  :description "Interval tree clocks for clojure/clojurescript"
  :url "https://github.com/csm/itc"
  :license {:name "LGPLv3"
            :url "https://www.gnu.org/licenses/lgpl-3.0.en.html"}
  :dependencies [[org.clojure/clojure "1.10.1" :scope "provided"]
                 [org.clojure/clojurescript "1.10.597" :scope "provided"]]
  :plugins [[lein-cljsbuild "1.1.7"]]
  :repl-options {:init-ns itc.repl}
  :cljsbuild
  {:test-commands {"unit-tests" ["node" "target/test.js"]}
   :builds [{:id                   "tests"
             :source-paths         ["src" "test"]
             :compiler {
                        :output-to     "target/test.js"
                        :optimizations :none
                        :target        :nodejs
                        :main          itc.core-test}}]})