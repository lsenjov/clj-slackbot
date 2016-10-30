(ns clj-slackbot.games.chess-test
  (:require [clj-slackbot.games.chess :refer :all]
            [clojure.test :refer :all]
            [clojure.spec :as s]
            )
  )

(def md-a
  "Metadata for player a"
  {:channel "#chess"
   :user "@a"
   }
  )

(def md-b
  "Metadata for player b"
  (assoc md-a :user "@b")
  )

;; Plays a game of chess
(testing "Normal game"
  (-> (bot-start)
      (join [] md-a)
      (join [] md-b)
      (start [] md-a)
      (move ["a2" "a4"] md-a)
      ;(move ["f7" "f6"] md-b)
      ;(#(s/explain ::clj-slackbot.games.chess/chessGame %))
      :message
      )
  )
