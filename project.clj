(defproject n01se/deltype "0.0.2"
            :description "Delegation support in deftype and reify. This is an
                         experimental augmentation to support automatic
                         delegation to specified fields with a specified
                         interface."
            :url "http://github.com/jclaggett/deltype"
            :license {:name "Eclipse Public License - v 1.0"
                      :url "http://www.eclipse.org/legal/epl-v10.html"
                      :distribution :repo
                      :comments "same as Clojure"}
            :min-lein-version "2.0.0"

            :dependencies [[org.clojure/clojure "1.6.0"]]


            ;; Describe a performance specific test profile
            :test-selectors {:default (complement :perf), :perf :perf}

            :profiles {:1.6 {:dependencies [[org.clojure/clojure "1.6.0"]]}
                       :1.5 {:dependencies [[org.clojure/clojure "1.5.1"]]}
                       :1.4 {:dependencies [[org.clojure/clojure "1.4.0"]]}
                       :1.3 {:dependencies [[org.clojure/clojure "1.3.0"]]}
                       :1.2 {:dependencies [[org.clojure/clojure "1.2.0"]]}}
            :aliases {"1.6" ["with-profile" "1.6"]
                      "1.5" ["with-profile" "1.5"]
                      "1.4" ["with-profile" "1.4"]
                      "1.3" ["with-profile" "1.3"]
                      "1.2" ["with-profile" "1.2"]}
            ;; Use this to allow YourKit to connect:
            ;; :jvm-opts ["-agentpath:yjp/libyjpagent.so"]
            )
