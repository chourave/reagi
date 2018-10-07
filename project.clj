(defproject chourave/reagi "0.11.0-SNAPSHOT"
  :description "An FRP library for Clojure and ClojureScript"
  :url "https://github.com/weavejester/reagi"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/core.async "0.3.465"]]
  :plugins [[lein-codox "0.10.4"]
            [lein-eftest "0.5.3"]]
  :source-paths ["src/cljc"]
  :test-paths ["test/clojure"]
  :codox {:sources ["src/cljc"]}
  :cljsbuild
  {:builds [{:source-paths ["src/cljc"]
             :compiler {:output-to "target/main.js"}}]}
  :eftest {:capture-output? false}
  :profiles
  {:cljs {:plugins [[com.cemerick/austin "0.1.6"]
                    [lein-cljsbuild "1.1.7"]
                    [lein-doo "0.1.10"]]}
   :clj-1.7   {:dependencies [[org.clojure/clojure        "1.7.0"        :scope "provided"]]}
   :cljs-1.7  {:dependencies [[org.clojure/clojurescript  "1.7.28"       :scope "provided"]]}
   :clj-1.8   {:dependencies [[org.clojure/clojure        "1.8.0"        :scope "provided"]]}
   :cljs-1.8  {:dependencies [[org.clojure/clojurescript  "1.8.51"       :scope "provided"]]}
   :clj-1.9   {:dependencies [[org.clojure/clojure        "1.9.0"        :scope "provided"]]}
   :cljs-1.9  {:dependencies [[org.clojure/clojurescript  "1.9.946"      :scope "provided"]]}
   :clj-1.10  {:dependencies [[org.clojure/clojure       "1.10.0-alpha8" :scope "provided"]]}
   :cljs-1.10 {:dependencies [[org.clojure/clojurescript "1.10.339"      :scope "provided"]]}
   :with-xml { ; Needed for cljs < 1.10 to work with java >= 9
              :jvm-opts ~(let [version     (System/getProperty "java.version")
                               [major _ _] (clojure.string/split version #"\.")]
                           (if (>= (java.lang.Integer/parseInt major) 9)
                             ["--add-modules" "java.xml.bind"]
                             []))}
   :dev  {:dependencies [[clj-async-test "0.0.5"]
                         [criterium "0.4.2"]
                         [eftest "0.5.3"]]
          :repl-options {:init (require '[reagi.core :as r]
                                        '[clojure.core.async
                                          :as a :refer [go go-loop <! >! <!! >!!]])}
          :doo     {:build "test-build"}
          :cljsbuild
          {:builds ^:replace {:test-build {:source-paths ["src/cljc" "test/cljs"]
                                           :compiler     {:output-to     "target/test.js"
                                                          :main          reagi.runner
                                                          :optimizations :simple}}}}}
   :default [:base :system :user :provided :dev :clj-1.8]}
  :aliases
  {"test-cljs" ["with-profile" "+cljs" "doo" "phantom" "once"]
   "test-all"  ["with-profile" "+cljs-1.8" "do" "test," "test-cljs"]})
