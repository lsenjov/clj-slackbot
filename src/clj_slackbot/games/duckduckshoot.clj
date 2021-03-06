(ns clj-slackbot.games.duckduckshoot
  (:require [taoensso.timbre :as log]
            [clojure.spec :as s]
            [clj-slackbot.helpers :refer :all]
            )
  (:gen-class)
  )

;; Spec Definitions
(s/def ::actionType (s/and keyword?
                           #((set [:shoot :duck :nothing]) %)
                           )
  )
(s/def ::player string?)
(s/def ::action (s/keys :req [::actionType]
                        :opt [::player]))
(s/def ::actions (s/map-of ::player ::action))
(s/def ::ducks integer?)
(s/def ::alive? boolean?)
(s/def ::playerStat (s/keys ::alive? ::ducks))
(s/def ::players (s/map-of ::player ::playerStat))
(s/def ::status (s/and keyword?
                       #((set [:waiting :playing :over]) %)
                       )
  )
(s/def ::message (s/or :string string?
                       :map (s/map-of keyword? string?)
                       :collection (s/coll-of (s/map-of keyword? string?))))
(s/def ::round integer?)
(s/def ::gameMap (s/keys :req [::status ::players ::actions ::round]
                         :req-un [::message]))

;; Pre-game and status
(defn bot-start
  "Returns an empty game map"
  []
  {::players {}
   ::status :waiting
   :message "Game created"}
  )
(defn join
  "Has the player join the game"
  [gameMap commands {user :user channel :channel :as metaData}]
  {:pre [(s/assert string? user)
         (s/assert string? channel)]
   }
  (log/info "join. Current players:" (get-in gameMap [::players]))
  (cond
    ;; Not waiting
    (not (= (::status gameMap) :waiting))
    (assoc gameMap :message "Game has already begun")
    ;; Already joined
    (get-in gameMap [::players user]) (assoc gameMap :message (str "You have already joined this game " user))
    ;; Add player
    :not-found (-> gameMap
                   (assoc-in [::players user] {::ducks 0 ::alive? true})
                   (assoc :message (str user " has joined the game."))
                   )
    )
  )
(defn leave
  "Leave if already in the game"
  [gameMap commands {user :user channel :channel :as metaData}]
  (if (and (get-in gameMap [::players user])
           (= :waiting (::status gameMap))
           )
    (-> gameMap
        ;; Remove the user
        (update-in [::players] dissoc user)
        ;; Returns message
        (assoc :message (str user " has left the game.")))
    (assoc :message (str user " was not playing anyway."))))

(defn- get-alive-players
  "Returns a list of all alive players"
  [{players ::players :as gameMap}]
  {:pre [(s/assert ::gameMap gameMap)]
   :post [(s/valid? (s/coll-of ::player) %)]}
  (log/trace "get-alive-players. Players:" players)
  (->> players
       (filter #(->> % val ::alive?))
       keys
       (into [])
       (#(do (log/trace "Alive players:" %)
             %))
       )
  )
(defn status
  "Show status of the current game"
  [gameMap commands {user :user channel :channel :as metaData}]
  (assoc gameMap :message
         (case (::status gameMap)
           :waiting (str "Waiting for players. Current players:"
                          (apply str
                                 (interpose ", " (keys (gameMap ::players)))))
           :playing (str "Waiting for players to shoot and duck!\n"
                         (get-alive-players gameMap))
           :over "Game finished!"
           "ERROR: Unknown Status")))
(defn- concat-new-round-messages
  "Concats new messages"
  [{players ::players :as gameMap} channel]
  {:pre [(s/assert ::gameMap gameMap)
         (s/assert string? channel)]
   :post [(s/assert ::gameMap %)]}
  (-> gameMap
      (concat-message (str "Round " (::turn gameMap) " begin."))
      (concat-message (apply str "Living players: " (interpose ", " (get-alive-players gameMap))))
      ((apply comp (for [p (get-alive-players gameMap)]
                     (fn [gm] (concat-message gm (str "Either `," channel " duck` or `," channel " shoot @player`. You have " (get-in gameMap [::players p ::ducks]) " ducks remaining.") p)))))
      )
  )
(defn- new-round
  "Associates :nothing actions to all alive players"
  [gameMap channel]
  {:pre [(s/assert ::gameMap gameMap)
         (s/assert string? channel)]
   :post [(s/assert ::gameMap %)]}
  (-> gameMap
      (assoc ::actions
             (apply merge {} (for [p (get-alive-players gameMap)]
                               {p {::actionType :nothing}})))
      (update-in [::round] inc)
      (concat-new-round-messages channel)
      )
  )
(defn- calculate-duck-count
  "Calculates the number of ducks a player should start with"
  ([^Integer n ^Integer players]
   (if (= 1 players)
     n
     (calculate-duck-count (inc n) (quot players 2))
     )
   )
  ([^Integer players]
   (calculate-duck-count 0 players)
   )
  )
(defn- start-set-ducks
  "Sets the number of ducks in ::players"
  [gameMap]
  {:pre [(s/assert ::gameMap gameMap)]
   :post [(s/assert ::gameMap %)]}
  (let [dc (calculate-duck-count (count (::players gameMap)))]
    (assoc gameMap
           ::players
           (apply merge {}
                  (for [[k v] (::players gameMap)]
                    {k (assoc v ::ducks dc)})
                  )
           )
    )
  )

(defn start
  "Actually starts the game"
  [gameMap commands {user :user channel :channel :as metaData}]
  {:post [(s/assert ::gameMap %)]}
  (log/trace "start. Players:" (into [] (::players gameMap)))
  (cond
       ;; Not enough players
       (< (count (::players gameMap)) 3)
       (assoc-message gameMap "Not enough players to start")
       ;; Game has already started
       (not (= :waiting (::status gameMap)))
       (assoc-message gameMap "Game has already begun")
       ;; Time to begin the game
       :start-game
       (-> gameMap
           (assoc ::actions {})
           ;; new-round increments this
           (assoc ::round 0)
           (start-set-ducks)
           (assoc ::status :playing)
           (new-round channel)
           )
       )
  )

(defn- player-action
  "Returns a function of a single player's action"
  [[player {actionType ::actionType target ::player}]]
  {:pre [(s/assert ::player player)
         (s/assert ::actionType actionType)
         (s/valid? (s/or :nil nil?
                         :player ::player)
                   target)]}
  (cond
    ;; Player failed to do anything
    (= actionType :nothing)
    (fn [gameMap]
      (concat-message gameMap (str player " did nothing.")))
    ;; Player shot at someone
    (= actionType :shoot)
    (fn [gameMap]
      (if (= :duck (get-in gameMap [::actions target ::actionType]))
        ;; Target ducked
        (concat-message gameMap (str player " shot at " target " but they ducked."))
        ;; Target was hit
        (-> gameMap
            (concat-message (str player " shot " target "."))
            (assoc-in [::players target ::alive?] false)
            )
        )
      )
    ;; Player ducked
    (= actionType :duck)
    (fn [gameMap]
      (-> gameMap
          (concat-message (str player " ducked."))
          (update-in [::players player ::ducks] dec)
          )
      )
    ;; Not found
    :not-found
    (fn [gameMap]
      (log/error "player-action. Should not reach here!")
      gameMap
      )
    )
  )

(defn- check-round-end
  "Checks if the game is to end. If true, ends the game. If false, triggers a new round."
  [gameMap channel]
  {:pre [(s/assert ::gameMap gameMap)]
   :post [(s/assert ::gameMap %)]}
  (let [playerCount (count (get-alive-players gameMap))]
    (cond
      ;; Everybody's dead dave
      (= playerCount 0)
      (-> gameMap
          (assoc ::status :over)
          (concat-message "Everybody's dead dave! Game over!")
          )
      ;; One or two players left
      (<= playerCount 2)
      (do
        (score-game (get-alive-players gameMap) (-> gameMap ::players keys))
        (-> gameMap
            (assoc ::status :over)
            (concat-message "Game over!")
            (concat-message (apply str "Winners are: " (interpose ", " (get-alive-players gameMap))))
            )
        )
      ;; Continue
      :next-round
      (new-round gameMap channel)
      )
    )
  )

(defn- end-round
  "Ends the round and calculates shots/ducks"
  [gameMap channel]
  {:pre [(s/assert ::gameMap gameMap)]
   :post [(s/assert ::gameMap %)]}
  (log/trace "end-round triggered.")
  (-> gameMap
      ((apply comp (map player-action (::actions gameMap))))
      (check-round-end channel)
      )
  )

(defn- trigger-action
  "Checks everyone has made an action, and if so, go wild"
  [gameMap channel]
  {:pre [(s/assert ::gameMap gameMap)]
   :post [(s/assert ::gameMap %)]}
  (->> gameMap
       ::actions
       (filter (fn [[k v]] (= (::actionType v) :nothing)))
       (count)
       (#(do (log/trace "trigger-action. Nothing counts:" %) %))
       (= 0)
       (
        #(if %
           (end-round gameMap channel)
           gameMap
           )
        )
       )
  )

(defn shoot
  "Take a shot at another player"
  [gameMap [target & _] {user :user channel :channel :as metaData}]
  {:pre [(s/assert ::gameMap gameMap)]
   :post [(s/assert ::gameMap %)]}
  (cond
    ;; Game hasn't started
    (not (= :playing (::status gameMap)))
    (assoc-message gameMap "Game is not in progress." user)
    ;; Not actually a player
    (not (get-in gameMap [::players user ::alive?]))
    (assoc-message gameMap "You are not alive to shoot!" user)
    ;; Target isn't alive
    (not (get-in gameMap [::players target ::alive?]))
    (assoc-message gameMap (str target " isn't a living player.") user)
    ;; Prepare the shot
    :good-shot
    (-> gameMap
        (assoc-in [::actions user]
                  {::actionType :shoot
                   ::player target}
                  )
        (assoc-message (str "Preparing to shoot " target) user)
        (trigger-action channel)
        )
    )
  )

(defn duck
  "Duck from people shooting at you."
  [gameMap [target & _] {user :user channel :channel :as metaData}]
  {:pre [(s/assert ::gameMap gameMap)]}
  (cond
    ;; Game hasn't started
    (not (= :playing (::status gameMap)))
    (assoc-message gameMap "Game is not in progress." user)
    ;; Not actually a player
    (not (get-in gameMap [::players user ::alive?]))
    (assoc-message gameMap "You are not alive to shoot!" user)
    ;; No ducks remaining
    (= 0 (get-in gameMap [::players user ::ducks]))
    (assoc-message gameMap "You can't duck any more!")
    ;; Duck!
    :duck
    (-> gameMap
        (assoc-in [::actions user ::actionType] :duck)
        (assoc-message "You are ducking." user)
        (trigger-action channel)
        )
    )
  )
