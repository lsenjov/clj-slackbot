(ns clj-slackbot.evaluator
  (:require [clojail.core :refer [sandbox]]
            [clojail.testers :refer [secure-tester-without-def blanket]]
            [taoensso.timbre :as log]
            [clj-slackbot.names :as names]
            [clojure.repl]

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
  (log/info "Attempting to start game:" gameType)
  (if (get gameMap channel)
    (assoc gameMap ;Game exists
           :message (str "Game already begun in this channel, gametype:"
                         (get-in gameMap [channel :gameType])))
    (try
      (let [gameNs (symbol (str "clj-slackbot.games." gameType))]
        (if-let [gameFn (ns-resolve gameNs 'bot-start)]
          (-> gameMap
              (assoc channel
                     (create-new-game gameType gameNs (gameFn)))
              (assoc :message "Game created"))
          (assoc gameMap :message "Gametype does not exist")))
        (catch Exception e (assoc gameMap :message "Gametype does not exist")))))

(defn end-game
  "Ends any game in progress in the channel"
  ([gameMap {channel :channel :as metaData}]
   (log/info "Ending game in channel:" channel)
   (if (get gameMap channel)
     (-> gameMap
         (dissoc channel)
         (assoc :message "Game ended"))
     (assoc gameMap :message "No game to end")
     )))

(defn send-command
  "Sends a command to a channel, if available"
  ([gameMap {channel :channel :as metaData} commands]
   (log/info "send-command. commands:" commands)
   (log/trace "send-command. gameMap:" gameMap)
   (if (get gameMap channel)
     (try
       (log/trace "send-commands. ns is:" (get-in gameMap [channel :nameSpace]))
       (if-let [gameFn (ns-resolve (get-in gameMap [channel :nameSpace])
                                (symbol (first commands)))]
         ;; If the function exists, go ahead
         (do 
           (log/trace "fn is:" gameFn)
           ;; Changes the actual bit of data
           (let [newMap (-> gameMap
                            ;; Clear the message queue in the game first
                            (assoc-in [channel :data :message] nil)
                            ;; Now perform the action
                            (update-in [channel :data] gameFn (rest commands) metaData))]
             ;; If we reach this point, the game hasn't thrown an uncaught exception
             ;; Moves the data up from the data structure to the top level for retrieval
             (log/info "send-commands: Has updated correctly, now moving message up")
             (assoc newMap :message (get-in newMap [channel :data :message])))
           )
         ;; Couldn't resolve function in namespace
         (assoc gameMap :message {:message (str "No command:" (first commands))})
         )
       (catch Exception e
         (do
           (log/debug "send-commands: Couldn't resolve namespace, caught exception:" e)
           (log/trace (clojure.stacktrace/print-stack-trace e))
           (assoc gameMap :message (str "Command threw exception:" (first commands)))))
       )
     (assoc gameMap :message {:message "No current game"})
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
  (log/trace "conv-to-list?:" i)
  (if (string? i)
    (recur {:message i})
    (if (sequential? i)
      i
      (list i))))

(defn possible-game-namespaces
  "Returns a list of strings of the last item of the game namespaces"
  []
  (log/trace "possible-game-namespaces.")
  (->> (all-ns)
       (map str)
       (filter (fn [s] (= "games" (second (clojure.string/split s #"\.")))))
       (map (fn [x] (last (clojure.string/split x #"\."))))
       ))

(defn get-help-in-namespace
  "Returns a map of all function names and descriptions"
  [nmspace]
  (log/trace "get-help-in-namespace:" nmspace)
  (try (->> nmspace
            ns-publics
            (map (fn [[k v]] [k (:doc (meta v))]))
            (sort-by first)
            (map (fn [[k v]] (str k
                                  ": "
                                  v
                                  "\n")))
            (apply str)
            )
       (catch Exception e (str "No gameType: "
                               (last (clojure.string/split
                                       (str nmspace)
                                       (re-pattern "\\.")))
                               ".\n"
                               "Possible gameTypes: "
                               (apply str (interpose " " (possible-game-namespaces)))
                               ))))

(defn get-help
  "Gets general help, or help for a certain game"
  [metaData words]
  (log/trace "get-help. words:" words)
  {:channel (:user metaData)
   :message (if (= (count words) 0)
              "Possible commands:
              `help`, `bot-help` This help message. Takes optional gameType.
              `bot-games` Lists possible game types
              `bot-start gameType` Starts a game of gameType
              `bot-restart gameType` Ends whatever game is in progress, and starts a game of gameType
              `bot-end` ends the current game
              `bot-repo` displays a link to the source repo"
              (get-help-in-namespace (symbol (str "clj-slackbot.games." (first words))))
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
  (log/trace "translating single id:" i)
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
  (log/trace "translate-commands:" in)
  (let [words (clojure.string/split in #" ")]
    (map translate-single words)))

(defn- eval-expr-inner
  "Actually do something with the string. Does nothing right now"
  [{input :input {channel :channel user :user :as metaData} :meta :as s}]
  (log/info "eval-expr is:" s)
  (log/trace "eval-expr. channel is:" channel " user is:" user)
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
               "bot-restart" (do (swap! games end-game metaData)
                                 (swap! games start-game metaData (second words)))
               "bot-help" (get-help metaData (rest words))
               "help" (get-help metaData (rest words))
               "bot-games" (str "Possible gameTypes: "
                                (apply str (interpose " " (possible-game-namespaces))))
               "bot-repo" (repo-link metaData)
               "bot-init" (do (names/init) {:message "Reset channels and user id-name pairs."})
               "bot-sudo" (eval-expr-inner (-> s
                                               (assoc-in [:input] (apply str (interpose " " (rest (rest words)))))
                                               (assoc-in [:meta :user] (first (rest words)))))
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
