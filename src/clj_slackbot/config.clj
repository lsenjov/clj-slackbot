(ns clj-slackbot.config
  (:require [clojure.edn :as edn]
            [environ.core :refer [env]]))


(defn read-config
  "Reads config.edn to get the setup, or throws an exception if the file doesn't exist"
  []
  (let [path (or (:config-file env)
                 "config.edn")]
    (edn/read-string (slurp path))))
