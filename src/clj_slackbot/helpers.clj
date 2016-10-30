(ns clj-slackbot.helpers
  (:require [taoensso.timbre :as log]
            [clojure.edn]
            )
  (:gen-class)
  )

(defn p-trace
  "Passthrough trace. Logs to trace everything after the first arg. Returns the first arg"
  [& body]
  (log/trace (rest body))
  (first body)
  )

(defn t-trace
  "Thread trace. Logs whatever the first item is to trace, and returns it"
  [item & args]
  (log/trace "t-trace:" item "rest:" args)
  item)

;; All game namespaces use the :message syntax
(defn create-message
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
(defn assoc-message
  "Associates a message with the gameMap, removing any previous messages"
  ([gameMap message]
   (log/trace "assoc-message. message:" message)
   (assoc gameMap :message (create-message message)))
  ([gameMap message channel]
   (log/trace "assoc-message. message:" message "channel:" channel)
   (assoc gameMap :message (create-message message channel)))
  )
(defn concat-message
  "Adds messages to already existing messages"
  ([gameMap message]
   (log/trace "concat-message. message:" message)
   (update-in gameMap [:message] concat (create-message message)))
  ([gameMap message channel]
   (log/trace "concat-message. message:" message "channel:" channel)
   (update-in gameMap [:message] concat (create-message message channel)))
  )


;; Scores
(def ^:private starting-score
  "The starting score for a player to have"
  1000)
(def ^:private score-bet
  "The proportion of score bet each game."
  1/10)
(def ^:private score-filename
  "The filename to open and save to."
  "scores.edn")

(defn- current-time
  "Returns the current time in milliseconds since epoch"
  []
  (.getTimeInMillis (java.util.Calendar/getInstance)))

(defn- open-scores
  "Opens a scores file and gets the results, returns an empty map if it can't find the file"
  [f]
  (try (clojure.edn/read-string (slurp f))
       (catch java.io.FileNotFoundException e {})))

(def ^:private current-scores
  "The current scores in a map"
  (atom (open-scores score-filename)))

;; If scores exist, load them
(try (if-let [s (slurp "scores.edn")]
       (reset! current-scores (clojure.edn/read-string s)))
     (catch java.io.FileNotFoundException e nil))

(defn- save-scores
  "Opens a scores file and saves the results. Returns the results object"
  [f r]
  (log/info "save-scores. r:")
  (spit f (pr-str r))
  r)

(defn get-scores
  "Returns the current scores map"
  []
  @current-scores)

(defn- remove-score-single
  "Takes a score map, removes the correct proportion of score from a single player, adds the removed amount to :total"
  [user sMap]
  (log/trace "remove-score-single:" user sMap)
  (if-let [score (get-in sMap [user :score])]
    (let [amt (long (* score-bet score))]
      (log/trace "remove-score-single. amt:" amt)
      (-> sMap
          (update-in [:total] + amt)
          (update-in [user :score] + (- amt))
          (assoc-in [user :time] (current-time))
          ))
    (remove-score-single user (assoc-in sMap [user] {:score starting-score}))
    ))

(defn- add-score-to-player
  "Takes a score map, and adds the amount of score to each winning player"
  [amount user sMap]
  (log/trace "add-score-to-player:" user)
  (-> sMap
      (update-in [user :score] + amount)
      (assoc-in [user :time] (current-time))
      ))

(defn- add-score-to-winners
  "Takes a score map, divides the :score field and adds it to each of the winners"
  [sMap winners-list]
  (log/trace "add-score-to-winners. sMap:" sMap)
  (let [amt (quot (:total sMap) (count winners-list))]
    ((apply comp (map partial (repeat add-score-to-player) (repeat amt) winners-list)) sMap)))

(defn- score-game-inner
  "Takes a map of scores, and modifies the scores accordingly"
  [sMap winners-list all-players-list]
  (log/trace "score-game-inner.")
  (if (and (> (count winners-list) 0)
           (> (count all-players-list) 0))
    (-> sMap
        (assoc :total 0)
        ;; Remove all the bets from players, initialise new players
        ((apply comp (map partial (repeat remove-score-single) all-players-list)))
        (add-score-to-winners winners-list)
        ((partial save-scores score-filename))
        )
    (do
      (log/info "Either no winners or players. Winners:" (apply str winners-list) "All:" (apply str all-players-list))
      sMap)))

(defn score-game
  "Takes lists of winners and all players, and proceeds to modify scores accordingly"
  [winners-list all-players-list]
  (log/trace "score-game. winners:" winners-list "all-players:" all-players-list)
  (swap! current-scores score-game-inner winners-list all-players-list))
