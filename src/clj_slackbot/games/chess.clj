(ns clj-slackbot.games.chess
  (:require [taoensso.timbre :as log]
            [clojure.spec :as s]
            [clj-slackbot.helpers :refer :all]
            )
  (:gen-class)
  )

(def ^:private gamePieces
  "A map of game pieces and their slack representations"
  {nil
   {:black ":black_large_square:"
    :white ":white_large_square:"}
   :king
   {:black ":chess_king_black:"
    :white ":chess_king_white:"}
   :queen
   {:black ":chess_king_black:"
    :white ":chess_king_white:"}
   :bishop
   {:black ":chess_bishop_black:"
    :white ":chess_bishop_white:"}
   :knight
   {:black ":chess_knight_black:"
    :white ":chess_knight_white:"}
   :rook
   {:black ":chess_rook_black:"
    :white ":chess_rook_white:"}
   :pawn
   {:black ":chess_pawn_black:"
    :white ":chess_pawn_white:"}
   }
  )
(def ^:private gamePiecesChessfaq
  "A map of game pieces and their chessfaq representations"
  {
   :king
   {:black \k
    :white \K}
   :queen
   {:black \q
    :white \Q}
   :bishop
   {:black \b
    :white \B}
   :knight
   {:black \n
    :white \N}
   :rook
   {:black \r
    :white \R}
   :pawn
   {:black \p
    :white \P}
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
(s/def ::history (s/coll-of ::move))
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
  {:pre [(s/assert ::piece piece)
         ]
   :post [(s/assert ::chessGame %)]
   }
  (assoc-in gameMap [::pieces rank file] piece)
  )
(defn- get-piece
  "Gets a piece at a position on the gameboard"
  [gameMap {rank ::rank file ::file :as pos}]
  {:pre [(s/assert ::position pos)
         (s/assert ::chessGame gameMap)]
   :post [(s/valid? (s/or :piece ::piece :nil nil?)
                    %)]
   }
  (log/trace "get-piece. pos:" pos)
  (let [p (get-in gameMap [::pieces rank file])]
    (log/trace "get-piece. Return:" p)
    p
    )
  )

(defn- create-piece
  "Short way to create a piece from arguments"
  [colour pieceType rank file]
  {:post [(s/assert ::piece %)]}
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

(defn- print-piece
  "Returns the slack representation of the piece at x y"
  [gameMap x y]
  (if-let [{colour ::colour t ::type} (get-in gameMap [::pieces x y])]
    (get-in gamePieces [t colour])
    (get-in gamePieces [nil
                        (if (= 0 (rem (+ x y) 2))
                          :black
                          :white
                          )
                        ]
            )
    )
  )
(defn- print-board
  "Outputs a board in text format for slack"
  [gameMap]
  (apply str
         (for [y (range 8 0 -1)]
           (apply str \newline
                  (for [x (range 1 9)]
                    (print-piece gameMap x y)
                    )
                  )
           )
         )
  )

(defn- reduce-pieces-chessfaq
  "Taking a sequence of characters, reduces to a fenstring"
  ([charSeq]
   (reduce-pieces-chessfaq (list (first charSeq)) (rest charSeq)))
  ([s charSeq]
   {:pre [(s/assert (s/coll-of char?) s)
          (s/assert (s/or :charCollection (s/coll-of char?)
                          :string string?)
                    charSeq)]}
   (cond
     ;; Final
     (= 0 (count charSeq))
     (apply str s)
     ;; Nothing in s
     (= 0 (count s))
     (recur (concat s (list (first charSeq)))
            (rest charSeq))
     ;; First char of sequence is \1 (a nil) and last char is a number
     (let [c (int (last s))]
       (and (= \1 (first charSeq))
            (>= c (int \1))
            (<= c (int \8)))
       )
     (recur (concat (butlast s)
                    ;; Increment the character by one
                    (-> s last int inc char list))
            (rest charSeq))
     ;; Just add the character
     :otherwise
     (recur (concat s (list (first charSeq)))
            (rest charSeq))
     )
   )
  )
(defn- print-piece-chessfaq
  "Returns the character at the spot on the board"
  [gameMap x y]
  (if-let [{colour ::colour t ::type} (get-in gameMap [::pieces x y])]
    (get-in gamePiecesChessfaq [t colour])
    \1
    )
  )
(defn- print-file-chessfaq
  "Outputs a board file in fen99=items format"
  [gameMap ^Integer file]
  (apply str "fen" (- 19 file) \=
         (->> (map print-piece-chessfaq
                   (repeat gameMap)
                   (range 1 9)
                   (repeat file))
              (#(do (log/trace "file" file "is:" (into [] %)) %))
              (reduce-pieces-chessfaq '())
              )
         )
  )
(defn- print-board-chessfaq
  "Outputs a board in url format"
  [gameMap]
  (apply str "http://chessfaq.appspot.com/diagram?"
         (interpose \&
                    (map print-file-chessfaq
                         (repeat gameMap)
                         (range 1 9))
                    )
         )
  )

(defn join
  "Has the player join the game"
  [gameMap commands {user :user channel :channel :as metaData}]
  {:pre [(s/assert string? user)
         (s/assert string? channel)]
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


(defn- str-to-pos
  "Changes a string representation ('a5') to a position"
  [^String mov]
  {:pre [(s/assert (s/and string?
                          #(= 2 (count %)))
                   mov)]
   :post [(s/assert ::position %)]}
  {::rank (->> (first mov)
               (int)
               (#(- % (int \a)))
               (inc))
   ::file (- (int (second mov)) (int \0))}
  )
(defn- pos-to-str
  "Changes a position to a string representation ('a5')"
  [pos]
  {:pre [(s/assert ::position pos)]
   :post [(s/assert (s/and string?
                           #(= 2 (count %)))
                    %)]
   }
  (str (char (+ (dec (int \a)) (::rank pos)))
       (char (+ (int \0) (::file pos))))
  )
(defn- make-range
  "Makes a range from start (inclusive) to fin (exclusive), with a step of 1.
  Accounts for fin being less than start, and may make the step -1"
  [^Integer start ^Integer fin]
  (range start fin (if (> fin start) 1 -1))
  )

(defn- move-pawn-valid?
  "Is the specified movement valid? Returns a move if true, else nil"
  [gameMap
   {{fromX ::rank fromY ::file} ::position colour ::colour :as piece}
   {toX ::rank toY ::file :as positionTo}]
  {:pre [(s/assert ::chessGame gameMap)
         (s/assert ::piece piece)
         (s/assert ::position positionTo)]
   :post [(s/valid? (s/or :wrong nil? :valid ::move) %)]}
  (log/trace "move-pawn-valid?" piece positionTo)
  (cond
    ;; Moving one space forward
    ;; Same rank
    (and (= fromX toX)
         (= toY (+ fromY
                   ;; If it's white, move one forward
                   (if (= :white colour) 1 -1)
                   )
            )
         ;; Make sure there's no piece where it's moving to
         (nil? (get-piece gameMap positionTo))
         )
    (do
      (log/trace "move-pawn-valid. Move one space forward")
      {::turn (::turn gameMap)
       ::startPosition (::position piece)
       ::endPosition positionTo
       ::moveType :move}
      )
    ;; Moving two files in same rank
    (and (= fromX toX)
         (= (if (= colour :white) 2 7) fromY)
         (= (if (= colour :white) 4 5) toY))
    (do
      (log/trace "move-pawn-valid. Move two spaces forward")
      {::turn (::turn gameMap)
       ::startPosition (::position piece)
       ::endPosition positionTo
       ::moveType :move}
      )
    ;; Capturing
    (and (or (= fromX (dec toX))
             (= fromX (inc toX)))
         (= toY (+ fromY (if (= :white colour) 1 -1)))
         ;; Is the colour of the target the opposite of your colour? And does it actually exist?
         (= (::colour (get-piece gameMap positionTo))
            (if (= :white colour) :black :white))
         )
    (do
      (log/trace "move-pawn-valid?. Capturing")
      {::turn (::turn gameMap)
       ::startPosition (::position piece)
       ::endPosition positionTo
       ::moveType :capture}
      )
    ;; TODO en-passant
    :not-found
    (do
      (log/trace "move-pawn-valid? Invalid move")
      nil
      )
    )
  )

(defn- move-rook-valid?
  "Is the specified movement valid? Returns a move if true, else nil"
  [gameMap
   {{fromX ::rank fromY ::file} ::position colour ::colour :as piece}
   {toX ::rank toY ::file :as positionTo}]
  {:pre [(s/assert ::chessGame gameMap)
         (s/assert ::piece piece)
         (s/assert ::position positionTo)]
   :post [(s/valid? (s/or :wrong nil? :valid ::move) %)]}
  (cond
    ;; Must make sure it's moving in a straight line
    (not (or (= fromX toX)
             (= fromY toY)))
    nil
    ;; Final position is a friendly unit
    (= colour (::colour (get-piece gameMap positionTo)))
    nil
    ;; Moving horisontally
    (= fromX toX)
    (if (->>
          ;; Get the pieces along the way
          (map (fn [m x y] (get-piece m {::rank x ::file y}))
               (repeat gameMap)
               ;; Ignore this square
               (repeat toX)
               (rest (range fromY toY (if (> toY fromY) 1 -1)))
               )
          (remove nil?)
          (count)
          ;; If true, no pieces between us and target, horisontally
          (= 0)
          )
      {::turn (::turn gameMap)
       ::startPosition (::position piece)
       ::endPosition positionTo
       ::moveType (if (get-piece gameMap positionTo) :capture :move)}
      )
    ;; Moving vertically
    (= fromY toY)
    (if (->>
          ;; Get the pieces along the way
          (map (fn [m x y] (get-piece m {::rank x ::file y}))
               (repeat gameMap)
               ;; Ignore this square
               (rest (range fromX toX (if (> toX fromX) 1 -1)))
               (repeat toY)
               )
          (remove nil?)
          (count)
          ;; If true, no pieces between us and target, horisontally
          (= 0)
          )
      {::turn (::turn gameMap)
       ::startPosition (::position piece)
       ::endPosition positionTo
       ::moveType (if (get-piece gameMap positionTo) :capture :move)}
      )
    ;; Should never reach here
    :not-found
    (do
      (log/error "Rook-valid? Should never reach here.")
      nil)
    )
  )

(defn- move-bishop-valid?
  "Is the specified movement valid? Returns a move if true, else nil"
  [gameMap
   {{fromX ::rank fromY ::file} ::position colour ::colour :as piece}
   {toX ::rank toY ::file :as positionTo}]
  {:pre [(s/assert ::chessGame gameMap)
         (s/assert ::piece piece)
         (s/assert ::position positionTo)]
   :post [(s/valid? (s/or :wrong nil? :valid ::move) %)]}
  (cond
    ;; Is the target your own?
    (= colour (::colour (get-piece gameMap positionTo)))
    nil
    ;; Did it not move in a diagonal line?
    (not (= (Math/abs (- fromX toX))
            (Math/abs (- fromY toY))))
    nil
    ;; Are all the spaces clear?
    (->> (map (fn [m x y] (get-piece m {::rank x ::file y}))
              (repeat gameMap)
              (make-range fromX toX)
              (make-range fromY toY)
              )
         ;; First result will be this piece, remove it
         (rest)
         ;; All empties will be nil
         (remove nil?)
         (count)
         ;; If true, no pieces between us and target, horisontally
         (= 0)
         )
    {::turn (::turn gameMap)
     ::startPosition (::position piece)
     ::endPosition positionTo
     ::moveType (if (get-piece gameMap positionTo) :capture :move)}
    ;;Path isn't clear
    :pieces-blocking
    (do
      (log/trace "bishop-valid? pieces blocking.")
      nil
      )
    )
  )


(defn- remove-piece
  "Clears a position on the gameboard"
  [gameMap position]
  {:pre [(s/assert ::chessGame gameMap)
         (s/assert ::position position)]
   ;:post [(s/assert ::chessGame gameMap)]
   }
  (log/trace "remove piece. position:" position)
  (update-in gameMap [::pieces (::rank position)] dissoc (::file position))
  )

(defn- move-select
  "Given a correct piece, attempt to make a move"
  [gameMap positionFrom positionTo]
  {:pre [(s/assert ::chessGame gameMap)
        (s/assert ::position positionFrom)
        (s/assert ::position positionTo)]
   ;:post [(s/assert ::chessGame %)]
   }
  (log/trace "move-select. from/to:" positionFrom positionTo)
  (let [{colour ::colour t ::type :as piece} (get-piece gameMap positionFrom)]
    (if-let [m
             ;; Each of the below will return a ::move if valid, else nil
             (case t
               :pawn
               (move-pawn-valid? gameMap piece positionTo)
               :rook
               (move-rook-valid? gameMap piece positionTo)
               :bishop
               (move-bishop-valid? gameMap piece positionTo)
               nil
               )
             ]
      ;; Valid move!
      (do
        (log/trace "Valid move! Move is:" m)
        (-> gameMap
          (update-in [::history] conj m)
          (place-piece (assoc piece ::position positionTo))
          (#(do (log/trace "Piece placed") %))
          (remove-piece (::position piece))
          (#(do (log/trace "Old Piece removed") %))
          (update-in [::turn] inc)
          (assoc-message "Move successful")
          (#(concat-message % (print-board %)))
          )
        )
      ;; Invalid move
      (assoc-message gameMap "Invalid move")
      )
    )
  )

(defn start
  "Starts the game if there's enough players"
  [{players ::players :as gameMap} commands {user :user channel :channel :as metaData}]
  {:post [(s/assert ::chessGame %)]}
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
        (#(concat-message % (print-board %)))
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

(defn move
  "Make a move on your turn"
  [gameMap [firstMove secondMove] {user :user channel :channel :as metaData}]
  {:pre [(s/assert ::chessGame gameMap)
         (s/assert (s/and string?
                          #(= 2 (count %)))
                   firstMove)
         (s/assert (s/and string?
                          #(= 2 (count %)))
                   secondMove)
         ]
   :post [(s/assert ::chessGame %)]
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
    ;(catch AssertionError e (do (log/trace "move. Caught exception:" e) (throw e) (assoc-message gameMap "Invalid move")))
    )
  )
