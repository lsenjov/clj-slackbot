(ns clj-slackbot.games.werewolf
  (:require [taoensso.timbre :as log]
            [clj-slackbot.helpers :as helpers :refer :all]
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
          ;; Which side they win with
          :side :wolves
          ;; For :see actions
          :appears-as :wolves
          ;; Only roles with this are counted with regards to wolves outnumbering humans
          :count-win :wolves
          :action {:night {:kill nil}
                   :first-night {:inform true}
                   :day {:vote nil}}
          }
   :seer {:desc "You are the seer. You can see one player's alignment each night."
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
   :beholder {:desc "You are the beholder. You know who the seer is."
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
  "A list of extra roles possible."
  '(:tanner :beholder :pacifist :lycan :sorcerer :sympathiser)
  )

(def ^:private town-roles
  "A list of all extra roles on the town's side"
  (filter (fn [r] (= :town (-> all-roles r :side))) extra-roles))

(def ^:private wolf-roles
  "A list of all extra roles on the wolves' side"
  (let [part (filter (fn [r] (= :wolves (-> all-roles r :side))) extra-roles)]
    (concat part (repeat (count part) :wolf))))

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

(defn- get-count-by-side-win-condition
  "Returns a map of all alive players by role"
  [{players :players :as gameMap}]
  (log/trace "get-count-by-role. players:" players)
  (->> players
       (filter (fn [[_ {alive :alive}]] alive))
       (apply merge {})
       (group-by (fn [[_ {r :role}]] (get-in all-roles [r :count-win])))
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
           (filter (fn [[_ v]] (some #{action} (keys v))) actionMap) ;; Returns list of k-v vectors
           )))

(defn- count-votes
  "Returns a map of vote targets to the number of players that voted for it"
  [actions kw]
  (log/trace "count-votes:" actions kw)
  (-> (get-all-action-values actions kw)
      vals
      frequencies
      ))
(defn- show-votes
  "Returns a map of vote targets to a list of the players that voted for them"
  [actions kw]
  (log/trace "count-votes:" actions kw)
  (->> (get-all-action-values actions kw)
      (group-by (fn [[_ v]] v))
      (map (fn [[k v]] {k (map (fn [[nk _]] nk) v)}))
      (apply merge {})
      ))
(defn- display-votes
  "Converts the current daytime vote display to a pretty message"
  [{actions :actions :as gameMap}]
  (log/trace "display-votes. actions:" actions)
  (concat-message gameMap
                  (apply str
                         "Current vote totals:\n"
                         (->> (show-votes actions :vote)
                             (map (fn [[k v]] (str "Voting for: "
                                                   (if k k "Noone")
                                                   ": "
                                                   (apply str (interpose ", " v))
                                                   ".")))
                             (interpose \newline)))))

;; CREATE GAME
(defn bot-start
  "Returns an empty game map"
  []
  (log/trace "bot-start.")
  {:players {}
   :status :waiting
   :message "Game created"}
  )

;; JOINING GAMES
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

;; A PLAYER LEAVING A GAME
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

;; GAME STATUS
(defn- remove-not-nils
  "Removes items from a map where the value is truthy"
  [m]
  (apply merge {}
         (remove (fn [[k v]] v) m)))
(defn- list-unfilled-actions
  [{actions :actions :as gameMap}]
  (log/trace "list-unfilled-actions. actions:" actions)
  (->> actions
       (map (fn [[k v]] [k (remove-not-nils v)]))
       (remove (fn [[_ v]] (= 0 (count v))))
       (map (fn [[k v]] (keys v)))
       (apply concat)
       frequencies
       ))
(defn- formatted-unfilled-actions
  [{actions :actions :as gameMap}]
  (let [ufa (list-unfilled-actions gameMap)]
    (log/trace "formatted-unfilled-actions. ufa:" ufa)
    (str "Waiting on players to take actions:\n"
         (apply str
                (interpose \newline
                           (map (fn [[k v]] (str v " player(s) to " (name k)))
                                ufa))))))
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
    :night (assoc-message gameMap (formatted-unfilled-actions gameMap))
    :first-night (assoc-message gameMap (formatted-unfilled-actions gameMap))
    :end (assoc-message gameMap "Games is over.")
    (assoc-message gameMap "ERROR: Unknown Status")))

;; Creating a list of roles
(defn- fill-extra-roles
  "Returns a list of randomly selected roles from the extra roles, a random number of roles between 0 and the number of empty spots.
  Is a repeating sequence of two town roles, then one wolf role.
  Only adds extra roles if n >= 6, otherwise leaves it"
  [l ^Integer n]
  (log/trace "get-extra-roles:" l n)
  (concat l
          (take (quot n 6) (shuffle wolf-roles))
          (take (* 2 (quot n 6)) (shuffle town-roles))))
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
  (-> '(:seer :wolf)
      (fill-extra-roles n)
      (fill-villagers n)))

;; Assigning roles to players
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

;; Describing actions and roles
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

;; Appending actions and sending out action messages
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

;; START GAME
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

;; Checking victory conditions and triggering between phases
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
                 (concat-message (str n " was killed during the night."))
                 (concat-message (str n "'s role was:" (name (get-in gameMap [:players n :role]))))
                 )
             )
    ;; Nothing should be done here, just return the map
    gameMap))
(defn- show-summary
  "Shows a summary of the players in the game, along with their roles and their life status"
  [{players :players :as gameMap}]
  (->> players
       (map (fn [[n {r :role a :alive}]] (str n
                                              " -- "
                                              (name r)
                                              (if a ":white_check_mark:" ":finnadie:"))))
       (interpose \newline)
       (apply str)))
(defn- get-player-list
  "Gets the list of players from a gameMap"
  [{players :players :as gameMap}]
  (log/trace "get-player-list.")
  (keys players))
(defn- get-player-list-by-side
  [{players :players :as gameMap} side]
  (log/trace "get-player-list-by-side:" side)
  (->> players
       (filter (fn [[_ {r :role}]] (= side (-> all-roles r :side))))
       (map (fn [[k _]] k))))
(defn- score-by-side
  "Scores all players on one side, returns the original map anyway"
  [{players :players :as gameMap} side]
  (log/trace "score-by-side:" side)
  (helpers/score-game (get-player-list-by-side gameMap side)
                      (get-player-list gameMap))
  gameMap)
(defn- get-player-list-by-role
  [{players :players :as gameMap} role]
  (log/trace "get-player-list-by-role:" role)
  (->> players
       (filter (fn [[_ {r :role}]] (= role r)))
       (map (fn [[k _]] k))))
(defn- score-by-role
  "Scores all players of one role, returns the original map"
  [{players :players :as gameMap} role]
  (log/trace "score-by-role:" role)
  (helpers/score-game (get-player-list-by-role gameMap role)
                      (get-player-list gameMap))
  gameMap)
(defn- check-win-conditions
  "Checks to see if any side has won. If none, announce the next day."
  [{status :status players :players :as gameMap}]
  (log/trace "check-win-conditions. gameMap:" (pr-str gameMap))
  (let [{wolves :wolves town nil :as counts} (get-count-by-side-win-condition gameMap)]
    (let [wc (if wolves wolves 0)
          tc (if town town 0)]
      (log/trace "check-win-conditions. wc:" wc "tc:" tc)
      (if (= 0 wc)
        (-> gameMap
            (assoc :status :end)
            (concat-message "All the wolves are dead! The villagers win!")
            (concat-message (show-summary gameMap))
            (score-by-side :town)
            )
        (if (>= wc tc)
          (-> gameMap
              (assoc :status :end)
              (concat-message "The wolves outnumber the townsfolk! The wolves win!")
              (concat-message (show-summary gameMap))
              (score-by-side :wolves)
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
                (concat-message (show-summary gameMap))
                (score-by-role :tanner)
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
  (cond
    ;; User can't vote
    (not (has-action? gameMap user :vote))
    (assoc-message gameMap "You cannot vote right now")
    ;; User can't vote for themselves
    (= target user)
    (assoc-message gameMap "You cannot vote for yourself")
    ;; Clearing vote
    (= target "clear")
    (-> gameMap
        (set-role-action user :vote nil)
        (assoc-message (str user " cleared their vote."))
        (display-votes))
    ;; Not a valid target
    (not (is-alive? gameMap target))
    (assoc-message gameMap "Invalid target")
    ;; Valid
    :valid
    (-> gameMap
        (set-role-action user :vote target)
        (assoc-message (str user " has voted to lynch " target))
        (display-votes))
    )
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

;; Admin
(defn debug
  "Sends the current gamemap to logs. Does nothing in-game"
  [{players :players :as gameMap} commands {user :user channel :channel :as metaData}]
  (log/info "debug. gameMap:" (pr-str gameMap))
  (log/info "commands:" commands)
  (log/info "metaData:" metaData)
  (assoc-message gameMap "Sent current gamemap to logs")
  )

;; Describing roles
(def ^:private role-descriptions
  "Simple pre-compiled string for easy broadcast"
  (str "All possible roles (only villager, wolf, and seer appears at less that 6 players):\n"
       (apply str (interpose \newline
                             (map (fn [[k {d :desc}]] (str (name k) ": " d))
                                  all-roles)))))
(defn describe-roles
  "Sends the list of roles to the requesting player."
  [gameMap commands {user :user channel :channel :as metaData}]
  (log/info "describe-roles.")
  (assoc-message gameMap role-descriptions user))
