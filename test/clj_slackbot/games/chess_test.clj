(ns clj-slackbot.games.chess-test
  (:require [clj-slackbot.games.chess :refer :all :as chess]
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
(testing "Testing pawns"
  (-> (bot-start)
      (join [] md-a)
      (join [] md-b)
      (start [] md-a)
      (move ["a2" "a4"] md-a)
      (move ["b7" "b5"] md-b)
      (move ["a4" "b5"] md-a)
      (move ["a7" "a6"] md-b)
      (move ["a1" "a6"] md-a)
      (move ["c8" "a6"] md-b)
      ;(#(s/explain ::clj-slackbot.games.chess/chessGame %))
      :message
      ;::chess/history
      )
  )
