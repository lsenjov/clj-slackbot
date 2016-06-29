(ns clj-slackbot.evaluator
  (:require [clojail.core :refer [sandbox]]
            [clojail.testers :refer [secure-tester-without-def blanket]]
            [taoensso.timbre :as log]
            [clj-slackbot.names :as names]

            ;; Add games below here
            [clj-slackbot.games.testGame :as testGame]
            [clj-slackbot.games.werewolf :as werewolf]
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
  [gameMap {channel :channel :as metaData} gameType]
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
  ([gameMap {channel :channel :as metaData}]
   (if (get gameMap channel)
     (-> gameMap
         (dissoc channel)
         (assoc :message "Game ended"))
     (assoc gameMap :message "No game to end")
     )))

(defn send-command
  "Sends a command to a channel, if available"
  ([gameMap {channel :channel :as metaData} commands]
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

(defn conv-to-out?
  "If not already in a list, wraps in a list. If a string, sets is as the message"
  [i]
  (log/info "conv-to-list?:" i)
  (if (string? i)
    (recur {:message i})
    (if (sequential? i)
      i
      (list i))))

(defn get-help
  "Gets general help, or help for a certain game"
  [metaData words]
  {:channel (:user metaData)
   :message (if (= (count words) 0)
              "Possible commands:
              `,bot-start gameType` Starts a game of gameType
              `,bot-end` ends the current game
              `,bot-repo` displays a link to the source repo"
              (case (first words)
                "list" "GameTypes: testGame, werewolf" ;;TODO Add ways to automatically get all games
                (str "Unknown argument: "
                     (first words)
                     ". Possible arguments: list")
                )
              )
   }
  )

(defn repo-link
  [metaData]
  {:channel (:channel metaData)
   :message "https://github.com/lsenjov/clj-slackbot" }
  )

(defn- translate-single
  "If the command is in angular brackets, translate to actual name from ids"
  [^String i]
  (log/info "translating single id:" i)
  (if (and (= \< (first i))
           (= \> (last i)))
    (case (second i)
      \@ (names/convert-user-id (apply str (-> i rest rest butlast)))
      \# (names/convert-channel-id (apply str (-> i rest rest butlast)))
      i)
    i))

(defn- translate-commands
  "Splits commands by spaces, and changes usernames to actual names"
  [in]
  (let [words (clojure.string/split in #" ")]
    (map translate-single words)))

(defn- eval-expr-inner
  "Actually do something with the string. Does nothing right now"
  [{input :input {channel :channel user :user :as metaData} :meta :as s}]
  (log/info "eval-expr is:" s)
  (log/info "eval-expr. channel is:" channel " user is:" user)
  (if (= (first input) \#)
    ;; If the first argument is a channel, substitute the channel
    (recur (-> s
               ;; So we can find where this actually came from, not the modified one
               (assoc-in [:from] channel)
               (assoc-in [:meta :channel] (first (clojure.string/split input #" ")))
               (assoc-in [:input] (second (clojure.string/split input #" " 2)))
               ))
    ;; Split the words up
    (let [words (translate-commands input)]
      ;; For all the messages, by default they go to the named channel
      (map (partial merge {:channel channel :message "ERROR: No message"})
           ;; Convert to a list of maps, if not already in that form
           (conv-to-out?
             (case (first words)
               "bot-start" (:message (swap! games start-game metaData (second words)))
               "bot-end" (:message (swap! games end-game metaData))
               "bot-help" (get-help metaData (rest words))
               "help" (get-help metaData (rest words))
               "bot-repo" (repo-link metaData)
               (:message (swap! games send-command metaData words))
               )
             )
           )
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
