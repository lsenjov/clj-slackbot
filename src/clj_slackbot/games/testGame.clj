(ns clj-slackbot.games.testGame
  (:require [taoensso.timbre :as log]
            )
  )

(defn start
  "Returns an empty game map"
  []
  {:players []
   :message "Game created"}
  )

(defn testMessage
  "Does nothing, but returns a test message"
  [& data]
  (log/info "testMessage: input is:" data)
  (let [gameMap (first data)
        commands (second data)
        metaData (nth data 2)]
    (log/info "gameMap is:" gameMap)
    (log/info "commands are:" commands)
    (log/info "metaData are:" metaData)
    (if gameMap
      (assoc gameMap :message (str "Test message. Random number:" (rand-int 1000)))
      (assoc (start) :message "ERROR: Map was missing"))))
