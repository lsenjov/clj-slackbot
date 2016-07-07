(ns clj-slackbot.games.werewolf
  (:require [taoensso.timbre :as log]
            )
  (:gen-class)
  )

(def ^:private all-roles
  "A map of roles, their actions, and descriptions"
  {:villager {:desc "You are a villager"
              :side :town
              :appears-as :town
              :action {:day {:vote nil}}
              }
   :wolf {:desc "You are a werewolf"
          :side :wolves
          :appears-as :wolves
          :action {:night {:kill nil}
                   :first-night {:inform true}
                   :day {:vote nil}}
          }
   :seer {:desc "You are the seer."
          :side :town
          :appears-as :town
          :action {:night {:see nil}
                   :first-night {:see nil}
                   :day {:vote nil}}
          }
   :tanner {:desc "You are the tanner. You win alone if you are killed."
            :side :town
            :appears-as :town
            :action {:day {:vote nil}}
            }
   :beholder {:desc "You are the beholder."
              :side :town
              :appears-as :town
              :action {:first-night {:behold true}
                       :day {:vote nil}}
              }
   :pacifist {:desc "You are the pacifist, and refuse to vote to lynch a town member."
              :side :town
              :appears-as :town
              }
   :lycan {:desc "You are the lycan. You are on the side of the villagers, but appear as a wolf to the seer."
           :side :town
           :appears-as :wolves
           :action {:day {:vote nil}}
           }
   :sorcerer {:desc "You are a sorcerer. You are a seer, but are on the side of the werewolves."
              :side :wolves
              :appears-as :wolves
              :action {:night {:see nil}
                       :first-night {:see nil}
                       :day {:vote nil}}}
   :sympathiser {:desc "You are a wolf sympathiser. You are a normal human, but win with the werewolves, and show to the seer as a human."
                 :side :wolves
                 :appears-as :town
                 :action {:day {:vote nil}}
                 }
   }
  )

(def ^:private extra-roles
  "A list of extra roles possible"
  '(:tanner :beholder :pacifist :lycan :sorcerer :sympathiser)
  )

(def ^:private town-roles
  "A list of all extra roles on the town's side"
  (filter (fn [r] (= :town (-> all-roles r :side))) extra-roles))

(def ^:private wolf-roles
  "A list of all extra roles on the wolves' side"
  (filter (fn [r] (= :wolves (-> all-roles r :side))) extra-roles))

;; HELPERS
(defn- is-alive?
  "Checks a gamemap to see if the user is alive"
  [{players :players :as gameMap} user]
  (log/trace "is-alive? user:" user)
  (get-in players [user :alive]))

(defn- has-action?
  "Checks a gamemap to see if the user has the action. Returns true or nil"
  [{actions :actions :as gameMap} user action]
  (log/trace "has-action? user:" user "action:" action "gameMap:" gameMap)
  (if (some #{action} (keys (get actions user)))
    true
    nil))

(defn- get-action
  "Gets the current value of a player's action. May return nil if the action hasn't been taken yet. Assumes has been checked first with has-action"
  [{actions :actions :as gameMap} user action]
  (log/trace "get-action. user:" user "action:" action "gameMap:" gameMap)
  (-> actions
      (get user)
      (get action)))

(defn- is-alive-has-action?
  "Checks a gamemap to see if the user is alive and has the role. Returns true or nil"
  [{actions :actions players :players :as gameMap} user action]
  (log/trace "is-alive-has-action?. user:" user "role:" action "actions:" actions)
  (and (is-alive? gameMap user)
       (has-action? gameMap user action)))

(defn- get-count-by-role-all
  "Returns a map of all players by role, dead or alive"
  [{players :players :as gameMap}]
  (log/trace "get-count-by-role-all. players:" players)
  (->> players
       (group-by (fn [[_ {r :role}]] r))
       (map (fn [kv] {(key kv) (count (val kv))}))
       (apply merge {})
       )
  )

(defn- get-count-by-role
  "Returns a map of all alive players by role"
  [{players :players :as gameMap}]
  (log/trace "get-count-by-role. players:" players)
  (->> players
       (filter (fn [[_ {alive :alive}]] alive))
       (apply merge {})
       (group-by (fn [[_ {r :role}]] r))
       (map (fn [kv] {(key kv) (count (val kv))}))
       (apply merge {})
       )
  )

(defn- get-count-by-side
  "Returns a map of all alive players by role"
  [{players :players :as gameMap}]
  (log/trace "get-count-by-role. players:" players)
  (->> players
       (filter (fn [[_ {alive :alive}]] alive))
       (apply merge {})
       (group-by (fn [[_ {r :role}]] (get-in all-roles [r :side])))
       (map (fn [kv] {(key kv) (count (val kv))}))
       (apply merge {})
       )
  )

(defn- set-role-action
  "Sets a user's action if they have it. Does not send any messages about it."
  [{actions :actions :as gameMap} user action value]
  (log/trace "set-role-action. user:" user "action:" action "value:" value)
  (if (has-action? gameMap user action)
    (assoc-in gameMap [:actions user action] value)
    (do
      (log/debug "Did not set-role-action for:" user action value)
      gameMap)))

(defn- get-all-action-values
  "Returns a map of player to action value, for all players who have that action"
  [actionMap action]
  (apply merge {}
         (map 
           (fn [[n m]] {n (get m action)})
           (filter (fn [[_ v]] (get v action)) actionMap) ;; Returns list of k-v vectors
           )))

(defn- create-message
  "Wraps a message in a map in a list, if it's not already. Won't do anything if m is not a string or map"
  ([message]
  (if (string? message)
    (recur {:message message})
    (if (map? message)
      (list message)
      message)))
  ([message channel]
   (create-message {:message message :channel channel}))
  )

(defn- assoc-message
  "Associates a message with the gameMap"
  ([gameMap message]
   (log/trace "assoc-message. message:" message)
   (assoc gameMap :message (create-message message)))
  ([gameMap message channel]
   (log/trace "assoc-message. message:" message "channel:" channel)
   (assoc gameMap :message (create-message message channel)))
  )

(defn- concat-message
  "Adds messages to already existing messages"
  ([gameMap message]
   (log/trace "concat-message. message:" message)
   (update-in gameMap [:message] concat (create-message message)))
  ([gameMap message channel]
   (log/trace "concat-message. message:" message "channel:" channel)
   (update-in gameMap [:message] concat (create-message message channel)))
  )

(defn- count-votes
  "Returns a map of vote targets to number of votes"
  [actions kw]
  (-> (get-all-action-values actions kw)
      vals
      frequencies))
(defn- display-votes
  "Converts the current daytime vote display to a pretty message"
  [{actions :actions :as gameMap}]
  (concat-message gameMap
                  (apply str
                         "Current vote totals: "
                         (-> (count-votes actions :vote)
                             ((partial map (fn [[k v]] (str (if k k "Noone") ": " v ". "))))
                             ((partial interpose " "))))))

;; CREATE GAME
(defn bot-start
  "Returns an empty game map"
  []
  (log/trace "bot-start.")
  {:players {}
   :status :waiting
   :message "Game created"}
  )

(defn join
  "Has the player join the game"
  [{status :status :as gameMap} commands {user :user channel :channel :as metaData}]
  (log/trace "join. Current players:" (get-in gameMap [:players]))
  (if (= :waiting status)
    (if (get-in gameMap [:players user])
      (assoc gameMap :message (str "You have already joined this game " user))
      (do
        (-> gameMap 
            (assoc-in [:players user] {:alive true})
            (assoc-message (str user " has joined the game."))
            )))
    (assoc-message gameMap "The game has already started.")))

(def hiub "Has the player join the game" join)

(defn leave
  "Leave if already in the game"
  [{status :status :as gameMap} commands {user :user channel :channel :as metaData}]
  (log/trace "user leaving:" user)
  (if (= :waiting status)
    (if (get-in gameMap [:players user])
      (-> gameMap
          ;; Remove the user
          (update-in [:players] dissoc user)
          ;; Returns message
          (assoc-message (str user " has left the game.")))
      (assoc-message (str user " was not playing anyway.")))
    (assoc-message gameMap "The game has already started.")))

(defn status
  "Show status of the current game"
  [gameMap commands {user :user channel :channel :as metaData}]
  (log/trace "status.")
  (case (:status gameMap)
    :waiting (assoc-message gameMap
                            (str "Waiting for players. Current players: "
                                 (apply str
                                        (interpose ", " (keys (gameMap :players))))))
    ;; Have to dissoc, as display-votes concats to previous messages
    :day (display-votes (dissoc gameMap :message))
    :night (assoc-message gameMap "Not yet implemented.")
    :first-night (assoc-message gameMap "Waiting on seer.")
    :end (assoc-message gameMap "Games is over.")
    (assoc-message gameMap "ERROR: Unknown Status")))

(defn- fill-extra-roles
  "Returns a list of randomly selected roles from the extra roles, a random number of roles between 0 and the number of empty spots.
  Starts with 1 town role, then a repeating sequence of 1 wolf role and 2 town roles"
  [l ^Integer n]
  (log/trace "get-extra-roles:" l n)
  (let [tr (shuffle town-roles)
        tra (apply concat (partition-all 1 2 tr))
        trb (apply concat (partition-all 1 2 (rest tr)))]
  (concat l (take (- n (count l)) (interleave tra wolf-roles trb)))))

(defn- fill-villagers
  "Fills the remaining roles of l with :villager"
  [l ^Integer n]
  (log/trace "fill-villagers:" l n)
  (concat l (repeat (- n (count l))
                    :villager)))

(defn- create-roles
  "Create a list of (unshuffled) roles to be used in the game"
  [^Integer n]
  (log/trace "create-roles:" n)
  ;; Has a seer and one wolf, every 6 players adds another wolf
  (-> '(:seer)
      (concat (repeat (inc (quot n 6)) :wolf))
      (fill-extra-roles n)
      (fill-villagers n)))

(defn- assign-single-role
  "Assigns a single role to a player"
  [role [pName pMap]]
  (log/trace "assign-single-role. role:" role "pName:" pName)
  {pName (assoc pMap :role role)}
  )

(defn- assign-roles
  "Assigns roles to a map of players"
  [playerMap]
  (log/trace "assign-roles.")
  (let [roles (shuffle (create-roles (count playerMap)))]
    (apply merge (map assign-single-role roles playerMap))))

(defn- single-role-description
  "Creates a private message for a single role description"
  [[pName pMap]]
  (log/trace "single-role-description.")
  {:channel pName
   :message (get-in all-roles [(:role pMap) :desc])})

(defn- trigger-single-action-description
  "Creates a message for a single action, must be added to a list as a message"
  [gameMap player action]
  (log/trace "trigger-single-action-description. player:" player "action:" action)
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
              :behold (str "The seer is: " (first (keys (filter #(= (:role (val %)) :seer) (:players gameMap)))))
              :vote "You can vote to lynch someone. Type `,#channel vote @player` here, or `,vote @player` in the channel"
              "ERROR: Should not be showing up. Let lsenjov know"
              )
   }
  )

(defn- filter-actions
  "Creates a list of players with current actions"
  [{status :status players :players :as gameMap}]
  (log/trace "filter-actions. status:" status "players:" players)
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
  (log/trace "Creating actions for " (:status gameMap))
  (-> gameMap
      (assoc :actions (filter-actions gameMap))
      )
  )

(defn- trigger-roles
  "Returns a list of messages for roles"
  [{actions :actions :as gameMap}]
  (log/trace "trigger-roles. actions:" actions)
  (map trigger-single-action-description
       (repeat gameMap)
       (keys actions)
       (map (comp first first) (vals actions))))

(defn- trigger-roles-first-night
  "Sends role messages out, triggers seer"
  [gameMap]
  (log/trace "trigger-roles-first-night")
  (-> gameMap
      (assoc-message (map single-role-description (:players gameMap)))
      (concat-message (trigger-roles gameMap))
      (concat-message "The first night begins. Please wait while the seer looks at someone.\nPlease don't talk during the nighttimes.")
      ))

(defn- trigger-roles-other-times
  "Sends role messages out"
  [{status :status :as gameMap}]
  (log/trace "trigger-roles-other-times.")
  (-> gameMap
      (concat-message (trigger-roles gameMap))
      (concat-message (case status
                        :day "It is daybreak. Please `vote` on who is to be lynched today."
                        :night "It is nighttime. Please don't talk during the night."
                        :end "The game has ended."
                        "ERROR: Unknown game status to be calling this."))
      )
  )

(defn- begin-game
  "Assigns roles, sets the time to first-night"
  [gameMap]
  (log/info "Beginning game")
  (-> gameMap
      (assoc :status :first-night)
      (update-in [:players] assign-roles)
      (create-actions)
      (trigger-roles-first-night)))

(defn start
  "Starts the game if there's enough players"
  [gameMap commands {user :user channel :channel :as metaData}]
  (log/info "start.")
  (log/trace "players:" (:player gameMap))
  (if (= (:status gameMap) :waiting)
    (if (>= (count (:players gameMap)) 3)
      ;; TODO
      (begin-game gameMap)
      ;; Not enough players
      (assoc gameMap :message (str "Not enough players. Current players: " (count (:players gameMap))))
      )
    ;; Wrong phase
    (assoc gameMap :message "Incorrect game state to start game")))

(defn- check-incomplete?
  "Checks all tasks are complete. Return list of players being waited upon or nil if all done"
  [{actions :actions :as gameMap}]
  (log/trace "check-incomplete? actions:" actions)
  (let [r (map first
               (filter (fn [[k vMap]]
                         ;; If any of the items are actually true, return the mapping
                         (some identity
                               (map nil? (vals vMap))
                               )
                         )
                       actions))]
    ;; If there is no-one remaining to take an action, return nil, otherwise return the list
    (if (= 0 (count r))
      nil
      r)
    )
  )

(defn- max-votes
  "Counts a list of actions, returns a [name count] pair, which had the highest count.
  Must specify a keyword, so can use for both town/wolves"
  [actions kw]
  (-> (count-votes actions kw)
      ((partial sort-by val))
      last))

(defn- take-actions
  "Checks and resolves available actions.
  Assumes it is the correct time to change between day and night, before the change"
  [{status :status actions :actions :as gameMap}]
  (case status
    :day (let [[n c] (max-votes actions :vote)]
           ;; If majority is reached
           (if (> c
                  (quot (count (get-all-action-values actions :vote)) 2))
             (-> gameMap
                 (assoc-in [:players n :alive] false)
                 (concat-message (str n " was lynched.")))
             (concat-message gameMap "No majority was reached, no-one was lynched.")))
    :first-night gameMap ;; We already sent the seer their message
    :night (let [[n c] (max-votes actions :kill)]
             ;; Highest person gets deaded
             (-> gameMap
                 (assoc-in [:players n :alive] false)
                 (concat-message (str n " was killed during the night."))))
    ;; Nothing should be done here, just return the map
    gameMap))

(def tgm {:actions {"@c" {:inform true}
                    "@lsenjov" {:see "@b"}}
          :players {"@lsenjov" {:role :seer :alive true}
                    "@a" {:role :villager :alive true}
                    "@b" {:role :tanner :alive true}
                    "@c" {:role :wolf :alive true}}
         :status :day
         :message '({:channel "@lsenjov" :message "Player @b is on the side of the villagers."})})

(defn- check-win-conditions
  "Checks to see if any side has won. If none, announce the next day."
  [{status :status players :players :as gameMap}]
  (log/trace "check-win-conditions. gameMap:" gameMap)
  (let [{wolves :wolves town :town :as counts} (get-count-by-side gameMap)]
    (let [wc (if wolves wolves 0)
          tc (if town town 0)]
      (log/trace "check-win-conditions. wc:" wc "tc:" tc)
      (if (= 0 wc)
        (-> gameMap
            (assoc :status :end)
            (concat-message "All the wolves are dead! The villagers win!")
            ;; TODO Add end-game stats
            )
        (if (>= wc tc)
          (-> gameMap
              (assoc :status :end)
              (concat-message "The wolves outnumber the townsfolk! The wolves win!")
              ;; TODO Add end-game stats
              )
          (if (= (:tanner (get-count-by-role gameMap)) (:tanner (get-count-by-role-all gameMap)))
            ;; No-one has won, it is a new day
            (-> gameMap
                (concat-message (case status
                                  :first-night "It is now the first night."
                                  :night "It is now night time."
                                  :day "It is now daytime."
                                  :end "The game has ended."
                                  "ERROR: This shouldn't be showing here."))
                )
            (-> gameMap
                (assoc :status :end)
                (concat-message "The tanner has died! The tanner wins!")
                ;; TODO add end-game stats
                )
            ))))))

(defn- check-and-trigger-change
  "Checks for all actions complete. If true, change from night to day or day to night, re-do actions"
  [{status :status :as gameMap} commands {user :user channel :channel :as metaData}]
  (log/trace "check-and-trigger-change. gameMap:" gameMap)
  (if (check-incomplete? gameMap)
    ;; Tasks incomplete, ignore
    gameMap
    ;; Tasks complete, time to switch
    (do
      (log/info "Triggering change from status:" status)
      (let [newStatus (case status
                        :first-night :day
                        :night :day
                        :day :night
                        status)]
        (-> gameMap
            (take-actions)
            (assoc-in [:status] newStatus)
            (check-win-conditions)
            (create-actions)
            (trigger-roles-other-times)
            )
        )
      )
    )
  )

;; SEE
(defn- see-action
  "The user is actually the seer, and is allowed to do the action, let's do it"
  [{players :players :as gameMap} commands {user :user channel :channel :as metaData}]
  (log/trace "see-action. player:" user)
  (if-let [target (:role (players (first commands)))]
    (-> gameMap
        (assoc-in [:actions user :see] (first commands))
        (assoc-message {:channel user
                         :message (str "Player "
                                       (first commands)
                                       " is on the side of the "
                                       (case (-> all-roles (target) :side)
                                         :town "villagers."
                                         :wolves "werewolves."))}))
    (assoc-message gameMap {:channel user :message "Invalid target."})))
(defn- see-inner
  "Used by the seer"
  [{players :players :as gameMap} commands {user :user channel :channel :as metaData}]
  (log/trace "see-inner commands: " (apply str (interpose " " commands)))
  ;; This filter finds all alive seers (one or none), and gets the name of the possible result
  (let [seer (first (first (filter (fn [[k {alive :alive role :role}]]
                                     (and alive
                                          (= role :seer)))
                                   players)))]
    (log/trace "The seer (if they are alive) is:" seer)
    ;; If user has the action, and has not taken it yet
    (if (and (is-alive-has-action? gameMap user :see)
             (not (get-action gameMap user :see)))
      ;; TODO do stuff
      (see-action gameMap commands metaData)
      (assoc-message gameMap
                     "You can't take that action right now"
                     user)
      )
    )
  )
(defn see
  "Frontend for the seer. Calls seer, then triggers roles"
  [{players :players :as gameMap} commands {user :user channel :channel :as metaData}]
  (log/trace "see. user:" user)
  (-> gameMap
      (see-inner commands metaData)
      (check-and-trigger-change commands metaData)))

;; VOTE
(defn- vote-inner
  "Checks to see if voting is allowed at this point"
  [{players :players :as gameMap} [target] {user :user channel :channel :as metaData}]
  (log/trace "vote-inner. target:" target)
  (if (has-action? gameMap user :vote)
    (if (or (is-alive? gameMap target)
            (= "clear" target))
      (if (= "clear" target)
        (-> gameMap
            (set-role-action user :vote nil)
            (assoc-message (str user " cleared their vote."))
            (display-votes))
        (-> gameMap
            (set-role-action user :vote target)
            (assoc-message (str user " has voted to lynch " target))
            (display-votes))
        )
      (assoc-message gameMap "Invalid target")
      )
    (assoc-message gameMap "You cannot vote right now"))
  )
(defn vote
  "Vote for someone during the daytime. Call `vote clear` to remove your vote"
  [{players :players :as gameMap} commands {user :user channel :channel :as metaData}]
  (log/trace "vote. user:" user "target:" (first commands))
  (-> gameMap
      (vote-inner commands metaData)
      (check-and-trigger-change commands metaData)))

;; KILL
(defn- kill-inner
  "Makes sure it's a valid target"
  [{players :players :as gameMap} [target] {user :user channel :channel :as metaData}]
  (log/trace "kill-inner. target:" target "user:" user)
  (if (has-action? gameMap user :kill)
    (if (is-alive? gameMap target)
      (-> gameMap
          (set-role-action user :kill target)
          (assoc-message (str "You have voted to kill " target) user))
      (assoc-message gameMap "Invalid target." user))
    (assoc-message gameMap "You can't do that right now." user)))
(defn kill
  "Werewolves vote for someone during the nighttime."
  [gameMap [target :as commands] {user :user channel :channel :as metaData}]
  (log/trace "kill. user:" user "target:" target)
  (-> gameMap
      (kill-inner commands metaData)
      (check-and-trigger-change commands metaData)))


(defn debug
  "Sends the current gamemap to logs. Does nothing in-game"
  [{players :players :as gameMap} commands {user :user channel :channel :as metaData}]
  (log/info "debug. gameMap:" gameMap)
  (log/info "commands:" commands)
  (log/info "metaData:" metaData)
  (assoc-message gameMap "Done")
  )
