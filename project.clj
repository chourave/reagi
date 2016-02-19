(defproject reagi "0.11.0-SNAPSHOT"
  :description "An FRP library for Clojure and ClojureScript"
  :url "https://github.com/weavejester/reagi"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.clojure/core.async "0.2.374"]
                 [org.clojure/clojurescript "1.7.228"]]
  :plugins [[codox "0.6.7"]
            [lein-cljsbuild "1.1.2"]]
  :source-paths ["src/cljc"]
  :test-paths ["test/clojure"]
  :codox {:sources ["src/cljc"]}
  :cljsbuild
  {:builds [{:source-paths ["src/cljc"]
             :compiler {:output-to "target/main.js"
                        :optimizations :simple}}]}
  :profiles
  {:dev  {:plugins [[com.cemerick/austin "0.1.6"]]
          :dependencies [[criterium "0.4.2"]]
          :repl-options {:init (require '[reagi.core :as r]
                                        '[clojure.core.async
                                          :as a :refer [go go-loop <! >! <!! >!!]])}}
   :test {:plugins [[lein-doo "0.1.6"]]
          :doo     {:build "test-build"}
          :cljsbuild
          {:builds ^:replace {:test-build {:source-paths ["src/cljc" "test/cljs"]
                                           :compiler     {:output-to     "target/test.js"
                                                          :main          reagi.runner
                                                          :optimizations :simple}}}}}}
  :aliases
  {"test-cljs" ["with-profile" "test" "doo" "nashorn" "once"]
   "test-all"  ["do" "test," "test-cljs"]})
