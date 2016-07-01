(ns clj-slackbot.names
  (:require [taoensso.timbre :as log]
            [clj-slackbot.config :as config]
            [clj-slack.users :as sUsers]
            [clj-slack.channels :as sChans]
            [clj-slack.chat :as sChat]
            [clj-slack.im :as sIm]
            )
  (:gen-class)
  )

(def ^:private apiConfig
  "A config for use by clj-slack library. Set at runtime from the config file"
  {:token (:api-token (config/read-config)) :api-url "https://slack.com/api"}
  )

(def ^:private userList
  "A map of userIds to user names"
  (atom nil))

(def ^:private channelList
  "A map of channelIds to actual channel names"
  (atom nil))

(def ^:private backUserList
  "A map of names to original ids"
  (atom nil))

(def ^:private backChannelList
  "A map of channel names back to ids, includes usernames to ids"
  (atom nil))

(defn- add-user-to-lists
  "Adds a user object to both lists, overwriting if anything is there"
  [uMap]
  (let [uName (str \@ (:name uMap))
        uId (:id uMap)]
    (log/trace "Adding item:" uMap)
    (swap! userList assoc uId uName)
    (swap! backUserList assoc uName uId)))

(defn- add-channel-to-lists
  "Adds a channel map to both lists, overwriting if anything is there"
  [cMap]
  (let [cName (str \# (:name cMap))
        cId (:id cMap)]
    (swap! channelList assoc cId cName)
    (swap! backChannelList assoc cName cId)))

(defn init
  "Initialises the lists with current items on the server"
  []
  ;; Doing users first
  (if-let [ul (:members (sUsers/list apiConfig))]
    (doall (map add-user-to-lists ul))
    (log/error "Could not get user list from server!"))
  ;; Then do channels
  (if-let [cl (:channels (sChans/list apiConfig))]
    (doall (map add-channel-to-lists cl))
    (log/error "Could not get user list from server!")))

(init)

(defn- convert-generic
  "Converts user or channel ids into actual names, checking against the relevant parts"
  [nameAtom backNameAtom firstChar func selectKW ^String id]
  (log/trace "convert-generic. firstChar:" firstChar "id:" id)
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
          (swap! backNameAtom assoc na id)
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
  (partial convert-generic userList backUserList \@ clj-slack.users/info :user))

(def convert-channel-id
  "If the user is of form #name, return the name.
  Else, convert the provided string to a user name.
  If both fail, return the id"
  (partial convert-generic channelList backChannelList \# clj-slack.channels/info :channel))

(defn convert-user-back
  "Converts a user name or channel name back to its original id, or returns the name if could not be found"
  [^String n]
  (if-let [id (get @backUserList n)]
    ;; We found it, return the id
    (do
      (log/info "Converting back name:" n)
      id)
    ;; Couldn't find, return the original name
    (do
      (log/info "Couldn't convert back name:" n)
      n)
    ))

(defn convert-channel-back
  "Converts a user name or channel name back to a channel id, or returns the name if could not be found"
  [^String n]
  (log/trace "Converting channel back:" n)
  (if-let [id (get @backChannelList n)]
    ;; We found the channel, return the id
    (do
      (log/info "Converting back name:" n)
      id)
    ;; Couldn't find, return the original name
    (if (= (first n) \@)
      ;; We're trying to send a message to a user, try and convert it
      (if-let [cId (-> (sIm/open apiConfig (convert-user-back n)) :channel :id)]
        ;; We successfully opened/found a dm channel! Use it
        (do
          (log/info "Got DM channel to user" n)
          (swap! backChannelList assoc n cId)
          cId)
        (do
          (log/info "Couldn't convert back name:" n)
          n)
        )
      n
      )
    )
  )
