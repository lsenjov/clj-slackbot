(ns clj-slackbot.games.testGame
  (:require [taoensso.timbre :as log]
            )
  (:gen-class)
  )

(defn bot-start
  "Returns an empty game map"
  []
  {:players {}
   :message "Game created"}
  )

(defn testMessage
  "Does nothing, but returns a test message"
  ;; With the exception of start, all commands take 3 arguments.
  ;; The current gameMap, the extra commands as a list of strings, and the metadata
  ;; At the end, the :message part should be assigned whatever is being returned.
  ;; A string will be sent to the channel
  ;; a map with :channel and :message will return to that channel with that message (user names can be :channel)
  ;; or finally a list of :channel :message maps can be returns for multiple messages
  [gameMap commands {user :user channel :channel :as metaData}]
  (log/info "gameMap is:" gameMap)
  (log/info "commands are:" commands)
  (log/info "metaData are:" metaData)
  (if gameMap
    (assoc gameMap :message (str "Test message. Random number:"
                                 (rand-int 1000)
                                 ". Thanks for playing: "
                                 (:user metaData)
                                 "!"))
    (assoc (start) :message "ERROR: Map was missing")))

(defn testMultipleMessages
  "Sends a message to the channel for each of the additional arguments"
  [gameMap commands {user :user channel :channel :as metaData}]
  (if (> (count commands) 0)
    (assoc gameMap :message (map (fn [w] {:message w}) commands))
    (assoc gameMap :message
           (list
             {:message "Default message 1"}
             {:message "Default message 2"}
             )
           )
    )
  )

(defn join
  "Has the player join the game"
  [gameMap commands {user :user channel :channel :as metaData}]
  (log/info "Current players:" (get-in gameMap [:players]))
  (if (get-in gameMap [:players user])
    (assoc gameMap :message (str "You have already joined this game " user))
    (do
      (-> gameMap 
      (assoc-in [:players user] {:ready false})
      (assoc :message (str user " has joined the game."))
      ))))

(defn leave
  "Leave if already in the game"
  [gameMap commands {user :user channel :channel :as metaData}]
  (if (get-in gameMap [:players user])
    (-> gameMap
        ;; Remove the user
        (update-in [:players] dissoc user)
        ;; Returns message
        (assoc :message (str user " has left the game.")))
    (assoc :message (str user " was not playing anyway."))))

(defn status
  "Show status of the current game"
  [gameMap commands {user :user channel :channel :as metaData}]
  (assoc gameMap :message
         (case (:status gameMap)
           "waiting" (str "Waiting for players. Current players:"
                          (apply str
                                 (interpose ", " (keys (gameMap :players)))))
           "ERROR: Unknown Status")))

(defn begin
  "Starts the game if there's enough players"
  [gameMap commands {user :user channel :channel :as metaData}]
  (if (>= (count (:players gameMap)) 3)
    ;; TODO
    (assoc :message "Beginning game not yet implemented")
    ;; Not enough players
    (assoc :message (str "Not enough players. Current players: " (count (:players gameMap))))
    ))
