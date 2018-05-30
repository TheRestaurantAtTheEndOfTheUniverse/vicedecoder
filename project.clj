(defproject vicedecoder "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [byte-streams "0.2.3"]
                 [org.clojure/tools.cli "0.3.7"]
                 ]

  :profiles {:dev {:repl-options {:init-ns          vicedecoder.core
                                  :nrepl-middleware [cemerick.piggieback/wrap-cljs-repl]
                                  :timeout          120000 }

                   :dependencies [[binaryage/devtools "0.9.9"]
                                  [ring/ring-mock "0.3.2"]
                                  [ring/ring-devel "1.6.3"]
                                  [prone "1.5.1"]
                                  [figwheel-sidecar "0.5.15"]
                                  [org.clojure/tools.nrepl "0.2.13"]
                                  [com.cemerick/piggieback "0.2.2"]
                                  [pjstadig/humane-test-output "0.8.3"]

                                  ]

                   :plugins      [[lein-figwheel "0.5.15"]
                                  [lein-doo "0.1.10"]
                                  [cider/cider-nrepl "0.15.1"]
                                  [org.clojure/tools.namespace "0.3.0-alpha4"
                                   :exclusions [org.clojure/tools.reader]]
                                  [refactor-nrepl "2.3.1"
                                   :exclusions [org.clojure/clojure]]
                                  ]

                   :injections [(require 'pjstadig.humane-test-output)
                                (pjstadig.humane-test-output/activate!)]

                   }})
