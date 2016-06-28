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
  (let [config (config/read-config)
        inst-comm (fn []
                    (log/info ":: building com:" (:comm config))
                    (make-comm (:comm config) config))]
    (log/info ":: starting with config:" config)

    (go-loop [[in out stop] (inst-comm)]
      (log/info ":: waiting for input")
      (if-let [form (<! in)]
        (let [input (:input form)
              res (evaluator/eval-expr form)]
          (log/info ":: form >> " input)
          (log/info ":: => " res)
          ;(>! out (assoc form :evaluator/result res))
          (>! out res)
          (recur [in out stop]))

        ;; something wrong happened, re init
        (do
          (log/info ":: WARNING! The comms went down, going to restart.")
          (stop)
          (<! (async/timeout 3000))
          (inst-comm))))

    (.join (Thread/currentThread))))

