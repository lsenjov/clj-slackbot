(ns clj-slackbot.games.chess
  (:require [taoensso.timbre :as log]
            [clojure.spec :as s]
            [clj-slackbot.helpers :refer :all]
            )
  (:gen-class)
  )

(def ^:private gamePieces
  "A map of game pieces and their slack representations"
  {:square-black ":black_large_square:"
   :square-white ":white_large_square:"
   :king-black ":chess_king_black:"
   :king-white ":chess_king_white:"
   :queen-black ":chess_king_black:"
   :queen-white ":chess_king_white:"
   :bishop-black ":chess_bishop_black:"
   :bishop-white ":chess_bishop_white:"
   :knight-black ":chess_knight_black:"
   :knight-white ":chess_knight_white:"
   :rook-black ":chess_rook_black:"
   :rook-white ":chess_rook_white:"
   :pawn-black ":chess_pawn_black:"
   :pawn-white ":chess_pawn_white:"
   }
  )
(def ^:private pieceTypes
  "List of the different types of pieces"
  [:king
   :queen
   :bishop
   :knight
   :rook
   :pawn]
  )

(s/def ::status (s/and keyword?
                       #((set [:waiting :playing :over]) %)
                       )
  )
(s/def ::colour (s/or :black #(= % :black)
                      :white #(= % :white))
  )
(s/def ::colours (s/and
                   ;; All keys are either black or white, and vals are strings
                   ;; of player names
                   (s/map-of ::colour
                             string?)
                   ;; Has both :black and :white
                   #(= 2 (count %))
                   )
  )
;; The numbered turn it is, starts at 1
(s/def ::turn integer?)
(s/def ::rank (s/and integer?
                     #(>= % 1)
                     #(<= % 8)
                     ;#(>= (int %) (int \a))
                     ;#(<= (int %) (int \h))
                     )
  )
(s/def ::file (s/and integer?
                     #(>= % 1)
                     #(<= % 8)
                     )
  )
(s/def ::position (s/keys :req [::rank ::file]))

;; Move history
(s/def ::startPosition ::position)
(s/def ::endPosition ::position)
(s/def ::moveType (s/and keyword?
                         #((set [:castleKingside :castleQueenside
                                :move :capture])
                           %)
                         )
  )
(s/def ::move (s/keys :req [::turn ::startPosition
                            ::endPosition ::moveType]))
(s/def ::history (s/coll-of string?))
(s/def ::players (s/coll-of string?))
(s/def ::message (s/or :string string?
                       :map (s/map-of keyword? string?)
                       :collection (s/coll-of (s/map-of keyword? string?))))
(s/def ::piece (s/keys :req [::colour ::position ::type]))
(s/def ::pieces (s/map-of ::rank (s/map-of ::file ::piece)))
(s/def ::chessGame
  (s/keys :req [::players ::turn ::colours ::history ::pieces ::status]
          ;; Message doesn't have to be double colon, so we use opt-un
          :opt-un [::message]
          )
  )

(defn- place-piece
  "Places a piece on the gameboard"
  [gameMap {{rank ::rank file ::file} ::position :as piece}]
  {:pre [(s/valid? ::piece piece)
         ]
   ;;:post [(s/valid? ::chessGame %)]
   }
  (assoc-in gameMap [::pieces rank file] piece)
  )
(defn- get-piece
  "Gets a piece at a position on the gameboard"
  [gameMap {rank ::rank file ::file :as piece}]
  {:pre [(s/valid? ::position piece)
         (s/valid? ::chessGame gameMap)]
   :post [(s/valid? (s/or :piece ::piece
                         :nil nil?)
                    %)]
   }
  (get-in gameMap [::pieces rank file])
  )

(defn- create-piece
  "Short way to create a piece from arguments"
  [colour pieceType rank file]
  {:post (s/valid? ::piece %)}
  {::colour colour
   ::type pieceType
   ::position {::rank rank ::file file}}
  )

(def ^:private gameStart
  "The initial list of pieces to add"
  (concat
    ;; Row of white major pieces
    (map create-piece
         (repeat :white)
         [:rook :knight :bishop :king :queen :bishop :knight :rook]
         (range 1 9)
         (repeat 1)
         )
    ;; Row of 8 white pawns
    (map create-piece
         (repeat :white)
         (repeat :pawn)
         (range 1 9)
         (repeat 2)
         )
    ;; Row of 8 black pawns
    (map create-piece
         (repeat :black)
         (repeat :pawn)
         (range 1 9)
         (repeat 7)
         )
    ;; Row of black major pieces
    (map create-piece
         (repeat :black)
         [:rook :knight :bishop :king :queen :bishop :knight :rook]
         (range 1 9)
         (repeat 8)
         )
    )
  )

(defn bot-start
  "Returns an empty game map"
  []
  {::players {}
   ::status :waiting
   :message "Game created"}
  )

(defn join
  "Has the player join the game"
  [gameMap commands {user :user channel :channel :as metaData}]
  {:pre [(s/valid? string? user)
         (s/valid? string? channel)]
   }
  (log/info "join. Current players:" (get-in gameMap [::players]))
  (cond
    ;; Not waiting
    (not (= (::status gameMap) :waiting))
    (assoc gameMap :message "Game has already begun")
    ;; More than two players
    (>= (-> gameMap ::players count) 2) (assoc gameMap :message "Too many players")
    ;; Already joined
    (get-in gameMap [::players user]) (assoc gameMap :message (str "You have already joined this game " user))
    ;; Add player
    :not-found (-> gameMap
                   (update-in [::players] concat (list user))
                   (assoc :message (str user " has joined the game."))
                   )
    )
  )

(defn leave
  "Leave if already in the game"
  [gameMap commands {user :user channel :channel :as metaData}]
  (if (and (get-in gameMap [::players user])
           (= :waiting (::status gameMap))
           )
    (-> gameMap
        ;; Remove the user
        (update-in [::players] dissoc user)
        ;; Returns message
        (assoc :message (str user " has left the game.")))
    (assoc :message (str user " was not playing anyway."))))

(defn status
  "Show status of the current game"
  [gameMap commands {user :user channel :channel :as metaData}]
  (assoc gameMap :message
         (case (::status gameMap)
           "waiting" (str "Waiting for players. Current players:"
                          (apply str
                                 (interpose ", " (keys (gameMap ::players)))))
           "ERROR: Unknown Status")))

(defn- get-turn-colour
  "Gets the colour of the current player to move"
  [gameMap]
  (if (= 1
         (rem (gameMap ::turn)
              2
              )
         )
    :white
    :black
    )
  )

(defn start
  "Starts the game if there's enough players"
  [{players ::players :as gameMap} commands {user :user channel :channel :as metaData}]
  {:post [(s/valid? ::chessGame %)]}
  (if (= (count players) 2)
    ;; Create game
    (-> gameMap
        ;; Assign starting player
        (assoc ::turn 1)
        ;; First player to join is always white
        (assoc ::colours (apply merge {}
                                (map (fn [k v] {k v})
                                     '(:white :black)
                                     players
                                     )
                                )
               )
        (assoc ::history '())
        (assoc ::status :playing)
        ;; Add pieces
        (assoc ::pieces {})
        ((apply comp
                (map (fn [p]
                       (fn [m] (place-piece m p)
                         )
                       )
                     gameStart)
                )
         )
        (#(assoc-message % (str "Player " (-> % ::colours :white) " to begin.")))
        )
    ;; Not enough players
    (assoc-message gameMap
                   (str "Not enough players. Current players: "
                        (count (::players gameMap)
                               )
                        )
                   )
    )
  )

(defn- str-to-pos
  "Changes a string representation ('a5') to a position"
  [^String mov]
  {:pre [(s/valid? (s/and string?
                          #(= 2 (count %)))
                   mov)]
   :post [(s/valid? ::position %)]}
  {::rank (->> (first mov)
               (int)
               (#(- % (int \a)))
               (inc))
   ::file (- (int (second mov)) (int \0))}
  )
(defn- pos-to-str
  "Changes a position to a string representation ('a5')"
  [pos]
  {:pre [(s/valid? ::position pos)]
   :post [(s/valid? (s/and string?
                           #(= 2 (count %)))
                    %)]
   }
  (str (char (+ (dec (int \a)) (::rank pos)))
       (char (+ (int \0) (::file pos))))
  )

(defn- move-pawn
  "Attempts to move a pawn"
  [gameMap piece positionTo]
  {:pre [(s/valid? ::chessGame gameMap)
        (s/valid? ::piece piece)
        (s/valid? ::position positionTo)]
   :post [(s/valid? ::chessGame %)]}
  gameMap
  )
(defn- move-select
  "Given a correct piece, attempt to make a move"
  [gameMap positionFrom positionTo]
  {:pre [(s/valid? ::chessGame gameMap)
        (s/valid? ::position positionFrom)
        (s/valid? ::position positionTo)]
   :post [(s/valid? ::chessGame %)]}
  (let [{colour ::colour t ::type :as piece} (get-piece gameMap positionFrom)]
    (cond
      (= :pawn t)
      (move-pawn gameMap piece positionTo)
      :not-found
      (assoc-message gameMap "Invalid move")
      )
    )
  )

(defn move
  "Make a move on your turn"
  [gameMap [firstMove secondMove] {user :user channel :channel :as metaData}]
  {:pre [(s/valid? ::chessGame gameMap)
         (s/valid? (s/and string?
                          #(= 2 (count %)))
                   firstMove)
         (s/valid? (s/and string?
                          #(= 2 (count %)))
                   secondMove)
         ]
   :post [(s/valid? ::chessGame %)]
   }
  (log/trace "move. moves:" firstMove secondMove)
  (try
    (cond
      ;; Not the correct player
      (not (= user (get-in gameMap [::colours
                                    ;; White or black's turn to move?
                                    (get-turn-colour gameMap)]
                           )
              )
           )
      (assoc-message gameMap "Not your turn")
      ;; Invalid piece
      (not (= (::colour (get-piece gameMap (str-to-pos firstMove))) (get-turn-colour gameMap)))
      (assoc-message gameMap "Not one of your pieces!")
      ;; Try and do things with the piece TODO
      :continue
      (move-select gameMap
                   (str-to-pos firstMove)
                   (str-to-pos secondMove))
      )
    (catch AssertionError e
      (assoc-message gameMap "Invalid move"))
    )
  )
