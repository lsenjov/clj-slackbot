(ns clj-slackbot.core
  (:require [clj-slackbot.config :as config]
            [clj-slackbot.util :as util]
            [clj-slackbot.evaluator :as evaluator]
            [clojure.core.async :as async :refer [>! <! go go-loop]]
            [taoensso.timbre :as log]
            )
  (:import java.lang.Thread)
  (:gen-class))

(defn make-comm [id config]
  (let [f (util/kw->fn id)]
    (f config)))

(defn -main [& args]
  (log/merge-config! {:level :trace})
  (log/info "Logging info enabled")
  (log/info "Logging trace enabled")
  (let [config (config/read-config)
        inst-comm (fn []
                    (log/info "Building com:" (:comm config))
                    (make-comm (:comm config) config))]
    (log/info "Starting with config:" config)

    (go-loop [[in out stop] (inst-comm)]
             (log/info "Waiting for input")
             (if-let [form (<! in)]
               (let [input (:input form)
                     res (evaluator/eval-expr form)]
                 (log/info "Input: " input)
                 (log/info "Output: " (apply str res))
                 ; Do we have a list/vector of messages? If so, loop through and send each one through
                 (log/trace "Returning message.")
                 (if (sequential? res)
                   (loop [queue res]
                     (when (> (count queue) 0)
                       (>! out (first queue))
                       (recur (rest queue))))
                   (>! out res)
                   )
                 (log/trace "Message sent.")
                 (recur [in out stop]))

               ;; something wrong happened, re init
               (do
                 (log/info ":: WARNING! The comms went down, going to restart.")
                 (stop)
                 (<! (async/timeout 3000))
                 (inst-comm))))

    (.join (Thread/currentThread))))

