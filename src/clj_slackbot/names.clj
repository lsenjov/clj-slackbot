(ns clj-slackbot.names
  (:require [taoensso.timbre :as log]
            [clj-slackbot.config :as config]
            [clj-slack.users :as sUsers]
            [clj-slack.channels :as sChans]
            [clj-slack.chat :as sChat]
            )
  )

(def ^:private userList
  "A map of userIds to user names"
  (atom nil))

(def ^:private channelList
  "A map of channelIds to actual channel names"
  (atom nil))

(def ^:private backList
  "A map of names to original ids"
  (atom nil))

(def ^:private apiConfig
  "A config for use by clj-slack library. Set at runtime from the config file"
  {:token (:api-token (config/read-config)) :api-url "https://slack.com/api"}
  )


(defn- convert-generic
  "Converts user or channel ids into actual names, checking against the relevant parts"
  [nameAtom firstChar func selectKW ^String id]
  (if (= (first id) firstChar)
    ; First character matches, it's a name, return it
    id
    ; Assume it's an id instead
    (if-let [local (get @nameAtom id)]
      ; It exists in the local map
      local
      ; It doesn't exist, time to convert it
      (if-let [n (-> (func apiConfig id) selectKW :name)]
        ; Hooray, we converted the name. Add the character
        (let [na (str firstChar n)]
          (swap! nameAtom assoc id na)
          (swap! backList assoc na id)
          na)
        ; Could not find at all. Could be a private channel.
        (do
          (log/debug "Could not translate id:" id)
          id)
        )
      )
    )
  )

(def convert-user-id
  "If the user is of form @name, return the name.
  Else, convert the provided string to a user name.
  If both fail, return the id"
  (partial convert-generic userList \@ clj-slack.users/info :user))

(def convert-channel-id
  "If the user is of form #name, return the name.
  Else, convert the provided string to a user name.
  If both fail, return the id"
  (partial convert-generic channelList \# clj-slack.channels/info :channel))

(defn convert-back
  "Converts a user name or channel name back to its original id, or returns the name if could not be found"
  [^String n]
  (if-let [id (get @backList n)]
    ;; We found it, return the id
    (do
      (log/info "Converting back name:" n)
      id)
    ;; Couldn't find, return the original name
    (do
      (log/info "Couldn't convert back name:" n)
      n)
    ))
