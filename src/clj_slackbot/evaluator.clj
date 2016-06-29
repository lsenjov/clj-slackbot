(ns clj-slackbot.evaluator
  (:require [clojail.core :refer [sandbox]]
            [clojail.testers :refer [secure-tester-without-def blanket]]
            [clj-slackbot.games.testGame :as testGame]
            [taoensso.timbre :as log]
            [clj-slackbot.names :as names]
            )
  (:import java.io.StringWriter))

(def clj-slackbot-tester
  (conj secure-tester-without-def (blanket "clj-slackbot")))

(def sb (sandbox clj-slackbot-tester))

(def games "A map atom of games keyed by channel." (atom {:message ""}))

(defn create-new-game
  "Returns a blank game map"
  [gameType nameSpace data]
  (log/info "New game created. Gametype:" gameType "Namespace:" nameSpace)
  {:gameType gameType :nameSpace nameSpace :data data :message "Game created"}
  )

(defn start-game
  "Checks if a game is in the channel. If not, start the game"
  [gameMap channel gameType]
  (if (get gameMap channel)
    (assoc gameMap ;Game exists
           :message (str "Game already begun in this channel, gametype:"
                         (get-in gameMap [channel :gameType])))
    (try
      (let [gameNs (symbol (str "clj-slackbot.games." gameType))]
        (if-let [gameFn (ns-resolve gameNs 'start)]
          (-> gameMap
              (assoc channel
                     (create-new-game gameType gameNs (gameFn)))
              (assoc :message "Game created"))
          (assoc gameMap :message "Gametype does not exist")))
        (catch Exception e (assoc gameMap :message "Gametype does not exist")))))

(defn end-game
  "Ends any game in progress in the channel"
  ([gameMap channel]
   (if (get gameMap channel)
     (-> gameMap
         (dissoc channel)
         (assoc :message "Game ended"))
     (assoc gameMap :message "No game to end")
     )))

(defn send-command
  "Sends a command to a channel, if available"
  ([gameMap channel commands metaData]
   (log/info "send-commands. commands:" commands)
   (if (get gameMap channel)
     (try
       (log/info "send-commands. ns is:" (get-in gameMap [channel :nameSpace]))
       (let [gameFn (ns-resolve (get-in gameMap [channel :nameSpace])
                                (symbol (first commands)))]
         (log/info "fn is:" gameFn)
         ;; Changes the actual bit of data
         (let [newMap (update-in gameMap [channel :data] gameFn (rest commands) metaData)]
           (log/info "send-commands: Has updated correctly, now moving message up")
           ;; Moves the data up from the data structure to the top level for retrieval
           (assoc newMap :message (get-in newMap [channel :data :message])))
         )
       (catch Exception e
         (do
           (log/info "send-commands: Couldn't resolve namespace, threw exception")
           (assoc gameMap :message (str "No command available:" (first commands)))))
       )
     {:message "No current game"}
     )
   )
  )

(defn conv-metadata
  "Converts metadata in a message to #channel and @sender, instead of ids"
  [md]
  (-> md
      (update-in [:meta :channel] names/convert-channel-id)
      (update-in [:meta :user] names/convert-user-id)))

(defn- eval-expr-inner
  "Actually do something with the string. Does nothing right now"
  [{input :input {channel :channel user :user :as metaData} :meta :as s}]
  (log/info "eval-expr is:" s)
  (log/info "eval-expr. channel is:" channel " user is:" user)
  (let [words (clojure.string/split input #" ")]
    (list
      {:channel channel
       :message (case (first words)
                  "start" (:message (swap! games start-game channel (second words)))
                  "end" (:message (swap! games end-game channel))
                  (:message (swap! games send-command channel words metaData))
                  )
       }
      {:channel "@logansenjov"
       :message "Testing"}
      )
    )
  )

(defn eval-expr
  "Send off to the relevant game"
  [message]
  (log/info "eval-expr before conversion: message")
  (eval-expr-inner (conv-metadata message))
  )

(defn format-result [r]
  (if (:status r)
    (str "```"
         "=> " (:form r) "\n"
         (when-let [o (:output r)]
           o)
         (if (nil? (:result r))
           "nil"
           (:result r))
         "```")
    (str "```"
         "==> " (or (:form r) (:input r)) "\n"
         (or (:result r) "Unknown Error")
         "```")))
