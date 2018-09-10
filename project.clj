(defproject chourave/reagi "0.11.0-SNAPSHOT"
  :description "An FRP library for Clojure and ClojureScript"
  :url "https://github.com/weavejester/reagi"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.clojure/core.async "0.1.298.0-2a82a1-alpha"]
                 [org.clojure/clojurescript "0.0-3291"]]
  :plugins [[lein-codox "0.10.4"]
            [lein-cljsbuild "1.0.3"]
            [com.keminglabs/cljx "0.3.2"]]
  :source-paths ["src/clojure" "src/cljs"]
  :test-paths ["test/clojure"]
  :hooks [cljx.hooks]
  :cljx
  {:builds [{:source-paths ["src/cljx"]
             :output-path "target/classes"
             :rules :clj}
            {:source-paths ["src/cljx"]
             :output-path "target/classes"
             :rules :cljs}]}
  :codox {:sources ["target/classes"]}
  :cljsbuild
  {:builds [{:source-paths ["target/classes"]
             :compiler {:output-to "target/main.js"}}]}
  :profiles
  {:dev  {:plugins [[com.cemerick/austin "0.1.6"]]
          :dependencies [[criterium "0.4.2"]]
          :repl-options {:init (require '[reagi.core :as r]
                                        '[clojure.core.async
                                          :as a :refer [go go-loop <! >! <!! >!!]])}}
   :test {:plugins [[lein-doo "0.1.6"]]
          :dependencies [[clj-async-test "0.0.5"]]
          :doo     {:build "test-build"}
          :cljsbuild
          {:builds ^:replace {:test-build {:source-paths ["target/classes" "test/cljs"]
                                           :compiler     {:output-to     "target/test.js"
                                                          :main          reagi.runner
                                                          :optimizations :simple}}}}}}
  :aliases
  {"test-cljs" ["with-profile" "test" "doo" "phantom" "once"]
   "test-all"  ["do" "test," "test-cljs"]})
