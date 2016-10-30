(defproject clj-slackbot "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :min-lein-version "2.0.0"
  :dependencies [[org.clojure/clojure "1.9.0-alpha10"]
                 [org.clojure/core.async "0.1.346.0-17112a-alpha"]
                 [compojure "1.4.0"]
                 [clj-http "3.3.0"]
                 [cheshire "5.3.1"]
                 [environ "1.1.0"]
                 [stylefruits/gniazdo "0.4.1"]
                 [ring/ring-core "1.6.0-beta6"]
                 [ring/ring-jetty-adapter "1.6.0-beta6"]
                 [ring/ring-defaults "0.3.0-beta1"]
                 [http-kit "2.3.0-alpha1"]

                 ;; Logging
                 [com.taoensso/timbre "4.7.4"]

                 ;; Slack API
                 [org.julienxx/clj-slack "0.5.4"]
                 ]
  :plugins [[lein-ring "0.8.13"]]
  :ring {:handler clj-slackbot.handler/app}
  :uberjar-name "clj-slackbot.jar"
  :main clj-slackbot.core
  :profiles
  {
   :uberjar {:aot :all
             :jvm-opts ["-Dclojure.spec.compile-asserts=false"]}
   :dev {:repl-options {:init-ns clj-slackbot.core.handler}
         :dependencies [[javax.servlet/servlet-api "2.5"]
                        [ring-mock "0.1.5"]]}
   }
  )
