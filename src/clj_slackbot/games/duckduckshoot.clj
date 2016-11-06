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
(defn status
  "Show status of the current game"
  [gameMap commands {user :user channel :channel :as metaData}]
  (assoc gameMap :message
         (case (::status gameMap)
           "waiting" (str "Waiting for players. Current players:"
                          (apply str
                                 (interpose ", " (keys (gameMap ::players)))))
           "ERROR: Unknown Status")))

(defn- get-alive-players
  "Returns a list of all alive players"
  [{players ::players :as gameMap}]
  {:pre [(s/assert ::gameMap gameMap)]
   :post [(s/valid? (s/coll-of ::player) %)]}
  (->> players
       (filter second)
       keys
       (#(do (log/trace "Alive players:" (into [] %)) %))
       )
  )
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
                     (fn [gm] (concat-message gm (str "Either `," channel " duck` or `," channel " shoot @player`") p)))))
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

(defn shoot
  "Take a shot at another player"
  [gameMap commands {user :user channel :channel :as metaData}]
  {:pre [(s/assert ::gameMap gameMap)]}
  gameMap
  )
