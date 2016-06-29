(ns clj-slackbot.games.werewolf
  (:require [taoensso.timbre :as log]
            )
  (:gen-class)
  )

(defn start
  "Returns an empty game map"
  []
  {:players {}
   :status "waiting"
   :message "Game created"}
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
