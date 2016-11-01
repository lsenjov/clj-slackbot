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
(testing "Testing game"
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
      (move ["b1" "c3"] md-a)
      (move ["d7" "d6"] md-b)
      (move ["d2" "d3"] md-a)
      (move ["e8" "b5"] md-b)
      (move ["d1" "d2"] md-a)
      (move ["b5" "c5"] md-b)
      (move ["b2" "b3"] md-a)
      (move ["b8" "c6"] md-b)
      (move ["h2" "h3"] md-a)
      (move ["d8" "b8"] md-b)
      ;(#(s/explain ::clj-slackbot.games.chess/chessGame %))
      :message
      (second) (:message) (#(spit "output.txt" %))
      )
  )

