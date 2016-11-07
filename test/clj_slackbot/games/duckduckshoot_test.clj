(ns clj-slackbot.games.duckduckshoot-test
  (:require [clj-slackbot.games.duckduckshoot :refer :all :as dds]
            [clojure.test :refer :all]
            [clojure.spec :as s]
            )
  )

(def md-a
  "Metadata for player a"
  {:channel "#dds"
   :user "@a"
   }
  )

(def md-b
  "Metadata for player b"
  (assoc md-a :user "@b")
  )

(def md-c
  "Metadata for player c"
  (assoc md-a :user "@c")
  )

(def md-d
  "Metadata for player c"
  (assoc md-a :user "@d")
  )

;; Check asserts, so any failure to conform to spec is reported
(s/check-asserts true)

(testing "Testing game"
  (-> (bot-start)
      (join [] md-a)
      (join [] md-b)
      (join [] md-c)
      (assoc :message '({:message "Remove later"}))
      (start [] md-a)
      (shoot ["@b"] md-a)
      (duck [] md-b)
      (shoot ["@a"] md-c)

      :message
      )
  )
