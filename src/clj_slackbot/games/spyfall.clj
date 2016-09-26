(ns clj-slackbot.games.spyfall
  (:require [taoensso.timbre :as log]
            )
  (:gen-class)
  )

(def gameData
  "List of locations and possible roles"
  {:airplane
   {:name "Airplane"
    :roles ["First Class Passenger"
            "Air Marshall"
            "Mechanic"
            "Economy Class Passenger"
            "Stwardess"
            "Co-Pilot"
            "Captain"]}
   :bank {:name "Bank"
          :roles ["Armoured Car Driver"
                  "Manager"
                  "Consultant"
                  "Customer"
                  "Robber"
                  "Security Guard"
                  "Teller"]}
   :beach {:name "Beach"
           :roles ["Beach Waitress"
                   "Kite Surfer"
                   "Lifeguard"
                   "Thief"
                   "Beach Goer"
                   "Beach Photographer"
                   "Ice Cream Truck Driver"]}
   }
  )

(defn bot-start
  "Returns an empty game map"
  []
  {:players {}
   :message "Game created"}
  )

(defn join
  "Has the player join the game"
  [gameMap commands {user :user channel :channel :as metaData}]
  (log/info "Current players:" (get-in gameMap [:players]))
  (if (get-in gameMap [:players user])
    (assoc gameMap :message (str "You have already joined this game " user))
    (-> gameMap
        (assoc-in [:players user] {:ready false})
        (assoc :message (str user " has joined the game."))
        )))

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

(defn- give-roles
  "Assigns the list of roles to a playermap, plus a separate spy action"
  [playerMap roles]
  (let [players (shuffle (keys playerMap))]
    ((apply comp
           (conj
             ;; Functions to assign all the players, with the ability to accuse
             (map
               ;; Need to make a list of functions
               partial
               ;; Each call to partial will take the following function
               (repeat (fn [p role _playerMap]
                         (-> _playerMap
                             (assoc-in [p :role] role)
                             (assoc-in [p :actions] {:accuse true}))))
               ;; The first player is the spy, so the rest get the roles
               (rest players)
               (shuffle roles))
             ;; Adds the spy to the game, able to accuse and guess
             (fn [_playerMap]
               (-> _playerMap
                   (assoc-in [(first players) :role] "Spy")
                   (assoc-in [(first players) :actions] {:guess true :accuse true})))
             )
           )
     ;; Apply the list of functions to the playermap and return it
     playerMap)))

(defn- assign-roles
  "Creates one spy role, and fills the rest with a random place from gamedata"
  [gameMap]
  (let [location (rand-nth (keys gameData))
        ;; Roles do not include whoever is the spy
        roles (shuffle (-> gameData location :roles))
        ]
    (-> gameMap
        (assoc :location location)
        (update-in [:players] give-roles roles)
        )
    )
  )

(defn- create-start-message
  "Given a player and a location, creates a private message for
  each player. Checks for spy."
  [[playerName playerMap] location]
  (if (= "Spy" (:role playerMap))
    {:channel playerName
     :message "You are the spy! You must attempt to uncover the location."}
    {:channel playerName
     :message (str "You are the "
                   (:role playerMap)
                   " in the "
                   (get-in gameData [location :name])
                   ".")}
    )
  )

(defn- send-start-messages
  "Creates messages for each of the players"
  [gameMap]
  (assoc gameMap :message
         (map create-start-message
              (:players gameMap)
              (repeat (:location gameMap))
              )
         )
  )

(defn begin
  "Starts the game if there's enough players"
  [gameMap commands {user :user channel :channel :as metaData}]
  (if (>= (count (:players gameMap)) 3)
    (-> gameMap
        assign-roles
        send-start-messages
        )
    ;; Not enough players
    (assoc :message (str "Not enough players. Current players: " (count (:players gameMap))))
    ))
