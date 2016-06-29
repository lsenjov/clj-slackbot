(ns clj-slackbot.games.werewolf
  (:require [taoensso.timbre :as log]
            )
  (:gen-class)
  )

(def ^:private all-roles
  "A map of roles, their actions, and descriptions"
  {:villager {:desc "You are a villager"
              :side :town
              :action {:day {:vote nil}}
              }
   :wolf {:desc "You are a werewolf"
          :side :wolves
          :action {:night {:kill nil}
                   :first-night {:inform true}
                   :day {:vote nil}}
          }
   :seer {:desc "You are the seer."
          :side :town
          :action {:night {:see nil}
                   :first-night {:see nil}
                   :day {:vote nil}}
          }
   }
  )

(defn bot-start
  "Returns an empty game map"
  []
  {:players {}
   :status :waiting
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
      (assoc-in [:players user] {:alive true})
      (assoc :message (str user " has joined the game."))
      ))))

(def hiub join)

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
           :waiting (str "Waiting for players. Current players: "
                         (apply str
                                (interpose ", " (keys (gameMap :players)))))
           "ERROR: Unknown Status")))

(defn- create-roles
  "Create a list of roles to be used in the game"
  [n]
  ;; Has a seer and one wolf, every 6 players adds another wolf
  (let [part (concat '(:seer) (repeat (inc (quot n 6)) :wolf))]
    ;; Fill the rest with villagers
    (concat part
            (repeat (- n (count part))
                    :villager))))

(defn- assign-single-role
  "Assigns a single role to a player"
  [role [pName pMap]]
  {pName (assoc pMap :role role)}
  )

(defn- assign-roles
  "Assigns roles to a map of players"
  [playerMap]
  (let [roles (create-roles (count playerMap))]
    (apply merge (map assign-single-role roles playerMap))))

(defn- single-role-description
  "Creates a private message for a single role description"
  [[pName pMap]]
  {:channel pName
   :message (get-in all-roles [(:role pMap) :desc])})

(defn- trigger-single-action-description
  "Creates a message for a single action"
  [gameMap player action]
  {:channel player
   :message (case action
              :kill "Vote now to kill someone. Type `,#channel kill @player`"
              :see "You can see the role of someone. Type `,#channel see @player`"
              :inform (str "All wolves: "
                           (apply str
                                  (interpose ", "
                                             (keys (filter #(= (:role (val %)) :wolf) (:players gameMap)))
                                             )
                                  )
                           )
              "ERROR: Should not be showing up. Let lsenjov know"
              )
   }
  )

(defn- filter-actions
  "Creates a list of players with current actions"
  [{status :status players :players :as gameMap}]
  (apply merge {}
         (filter some?
                 (map (fn [[k v]] (let [actions (get-in all-roles [(:role v) :action status])]
                               (if (and actions (:alive v))
                                 {k actions}
                                 nil)))
                      players))))

(defn- create-actions
  "Associates the new actions for the phase, and appends messages"
  [gameMap]
  (assoc gameMap :actions (filter-actions gameMap)))

(defn- trigger-roles
  "Returns a list of messages for roles"
  [{actions :actions :as gameMap}]
  (map trigger-single-action-description
       (repeat gameMap)
       (keys actions)
       (map (comp first first) (vals actions))))

(defn- trigger-roles-first-night
  "Sends role messages out, triggers seer"
  [gameMap]
  ;; TODO Seer ability
  (assoc gameMap :message
         (concat (map single-role-description (:players gameMap))
                 (trigger-roles gameMap)
                 )
         )
  )

(defn- begin-game
  "Assigns roles, sets the time to first-night"
  [gameMap]
  (-> gameMap
      (assoc :status :first-night)
      (update-in [:players] assign-roles)
      (create-actions)
      (trigger-roles-first-night)))

(defn begin
  "Starts the game if there's enough players"
  [gameMap commands {user :user channel :channel :as metaData}]
  (if (= (:status gameMap) :waiting)
    (if (>= (count (:players gameMap)) 3)
      ;; TODO
      (begin-game gameMap)
      ;; Not enough players
      (assoc :message (str "Not enough players. Current players: " (count (:players gameMap))))
      )
    ;; Wrong phase
    (assoc :message "Incorrect game state to start game")))

(defn check-complete?
  "Checks all tasks are complete. Return list of players being waited upon or false"
  [{actions :actions :as gameMap}]
  (map first
       (filter (fn [[k vMap]]
                 ;; If any of the items are actually true, return the mapping
                 (some identity
                       (map nil? (vals vMap))
                       )
                 )
               actions)
       )
  )

(defn- see-action
  "The user is actually the seer, and is allowed to do the action, let's do it"
  [{players :players :as gameMap} commands {user :user channel :channel :as metaData}]
  (if-let [target (:role (players (first commands)))]
    (-> gameMap
        (assoc-in [:actions user :see] (first commands))
        (assoc :message {:channel user
                         :message (str "Player "
                                       (first commands)
                                       " is on the side of the "
                                       (case (-> all-roles (target) :side)
                                         :town "villagers."
                                         :wolves "werewolves."))}))
    (assoc gameMap :message {:channel user :message "Invalid target."})))


(defn see
  "Used by the seer"
  [{players :players :as gameMap} commands {user :user channel :channel :as metaData}]
  (log/info "see commands: " (apply str (interpose " " commands)))
  ;; This filter finds all alive seers (one or none), and gets the name of the possible result
  (let [seer (first (first (filter (fn [[k {alive :alive role :role}]]
                              (and alive
                                   (= role :seer)))
                            players)))]
    (if (and (= seer user)
             ;; If the seer has already looked at someone, can't do it again
             (-> gameMap :actions (get user) :see not))
      ;; TODO do stuff
      (see-action gameMap commands metaData)
      (assoc gameMap :message {:channel user
                               :message "You can't take that action right now"})
      )
    )
  )
