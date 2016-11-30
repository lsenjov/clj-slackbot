(ns clj-slackbot.games.spyfall
  (:require [taoensso.timbre :as log]
            [clj-slackbot.helpers :as helpers]
            )
  (:gen-class)
  )

(def ^:private gameData
  "List of locations and possible roles"
  {:airplane {:name "Airplane"
              :roles ["First Class Passenger" "Air Marshall" "Mechanic" "Economy Class Passenger" "Stwardess" "Co-Pilot" "Captain"]}
   :bank {:name "Bank"
          :roles ["Armoured Car Driver" "Manager" "Consultant" "Customer" "Robber" "Security Guard" "Teller"]}
   :beach {:name "Beach"
           :roles ["Beach Waitress" "Kite Surfer" "Lifeguard" "Thief" "Beach Goer" "Beach Photographer" "Ice Cream Truck Driver"]}
   :casino {:name "Casino"
            :roles ["Bartender" "Head Security Guard" "Bouncer" "Manager" "Hustler" "Dealer" "Gambler"]}
   :cathedral {:name "Cathedral"
               :roles ["Priest" "Begger" "Sinner" "Parishioner" "Tourist" "Sponsor" "Choir Singer"]}
   :circus {:name "Circus"
            :roles ["Acrobat" "Animal Trainer" "Magician" "Visitor" "Fire Eater" "Clown" "Juggler"]}
   :corp-party {:name "CorporateParty"
                :roles ["Entertainer" "Manager" "Unwelcomed Guest" "Owner" "Secretary" "Accountant" "Delivery Boy"]}
   :crusade {:name "Crusade"
             :roles ["Servant" "Bishop" "Squire" "Archer" "Monk" "Imprisoned Arab" "Knight"]}
   :spa {:name "Spa"
         :roles ["Customer" "Stylist" "Masseuse" "Manicurist" "Makeup Artist" "Dermatologist" "Beautician"]}
   :embassy {:name "Embassy"
             :roles ["Security Guard" "Secretary" "Ambassador" "Government Official" "Tourist" "Refugee" "Diplomat"]}
   :hospital {:name "Hospital"
              :roles ["Nurse" "Doctor" "Anesthesiologist" "Intern" "Patient" "Therapist" "Surgeon"]}
   :hotel {:name "Hotel"
           :roles ["Doorman" "Security Guard" "Manager" "Housekeeper" "Customer" "Bartender" "Bellman"]}
   :military-base {:name "MilitaryBase"
                   :roles ["Deserter" "Colonel" "Medic" "Soldier" "Sniper" "Officer" "Tank Engineer"]}
   :studio {:name "Movie Studio"
            :roles ["Stuntman" "Sound Engineer" "Cameraman" "Director" "Costume Artist" "Actor" "Producer"]}
   :cruise-ship {:name "CruiseShip"
                 :roles ["Rich Passenger" "Cook" "Captain" "Bartender" "Musician" "Waiter" "Mechanic"]}
   :passenger-train {:name "PassengerTrain"
           :roles ["Mechanic" "Border Patrol" "Train Attendant" "Passenger" "Restaurant Chef" "Engineer" "Stoker"]}
   :pirate-ship {:name "PirateShip"
                 :roles ["Cook" "Sailor" "Slave" "Canoneer" "Bound Prisoner" "Cabin Boy" "Brave Captain"]}
   :polar-station {:name "PolarStation"
                   :roles ["Medic" "Geologist" "Expedition Leader" "Biologist" "Radioman" "Hydrologist" "Meteorologist"]}
   :police-station {:name "PoliceStation"
                    :roles ["Detective" "Lawyer" "Journalist" "Criminalist" "Archivist" "Patrol Officer" "Criminal"]}
   :restaurant {:name "Restaurant"
                :roles ["Musician" "Customer" "Bouncer" "Hostess" "Head Chef" "food Critic" "Waiter"]}
   :school {:name "School"
            :roles ["Gym Teacher" "Student" "Principal" "Security Guard" "Janitor" "Lunch Lady" "Maintenance Man"]}
   :service-station {:name "ServiceStation"
                     :roles ["Manager" "Tire Specialist" "Biker" "Car Owner" "Car Wash Operator" "Electrician" "Mechanic"]}
   :space-station {:name "SpaceStation"
                   :roles ["Engineer" "Alien" "Space Tourist" "Pilot" "Commander" "Scientist" "Doctor"]}
   :submarine {:name "Submarine"
               :roles ["Cook" "Commander" "Sonar Technician" "Electronics Technician" "Sailor" "Radioman" "Navigator"]}
   :supermarket {:name "Supermarket"
                 :roles ["Customer" "Cashier" "Butcher" "Janitor" "Security Guard" "Food Sample Demonstrator" "Shelf Stocker"]}
   :broadway {:name "BroadwayTheater"
              :roles ["Cashier" "Visitor" "Director" "Actor" "Coat Check Lady" "Prompter" "Crewman"]}
   :university {:name "University"
                :roles ["Graduate Student" "Professor" "Dead" "Psychologist" "Maintenance Man" "Student" "Janitor"]}
   }
  )

(defn bot-start
  "Returns an empty game map"
  []
  {:players {}
   :status :waiting
   :message "Game created"}
  )

(defn join
  "Has the player join the game"
  [gameMap commands {user :user channel :channel :as metaData}]
  (log/info "Current players:" (get-in gameMap [:players]))
  (if (get-in gameMap [:players user])
    (assoc gameMap :message (str "You have already joined this game " user))
    (-> gameMap
        (assoc-in [:players user] {:ready false})
        (assoc :message (str user " has joined the game."))
        )))

(defn leave
  "Leave if already in the game"
  [gameMap commands {user :user channel :channel :as metaData}]
  (if (get-in gameMap [:players user])
    (-> gameMap
        ;; Remove the user
        (update-in [:players] dissoc user)
        ;; Returns message
        (assoc :message (str user " has left the game.")))
    (assoc :message (str user " was not playing anyway."))))

(defn status
  "Show status of the current game"
  [gameMap commands {user :user channel :channel :as metaData}]
  (assoc gameMap :message
         (case (:status gameMap)
           :waiting (str "Waiting for players. Current players:"
                          (apply str
                                 (interpose ", " (keys (gameMap :players)))))
           :playing (if (:accusing gameMap)
                      (str "There is an accusation against "
                           (get-in gameMap [:accusing :target])
                           ".\n"
                           "Still to vote:"
                           (apply str (interpose ", " (get-in gameMap [:accusing :remaining]))))
                      (str "Game is underway.\n"
                           "Players: "
                           (apply str (interpose ", " (keys (gameMap :players))))
                           )
                      )
           :ended "The game has ended."
           "ERROR: Unknown Status")
         )
  )

(defn- give-roles
  "Assigns the list of roles to a playermap, plus a separate spy action"
  [playerMap roles]
  (let [players (shuffle (keys playerMap))]
    ((apply comp
           (conj
             ;; Functions to assign all the players, with the ability to accuse
             (map
               ;; Need to make a list of functions
               partial
               ;; Each call to partial will take the following function
               (repeat (fn [p role _playerMap]
                         (-> _playerMap
                             (assoc-in [p :role] role)
                             (assoc-in [p :actions] {:accuse true}))))
               ;; The first player is the spy, so the rest get the roles
               (rest players)
               (shuffle roles))
             ;; Adds the spy to the game, able to accuse and guess
             (fn [_playerMap]
               (-> _playerMap
                   (assoc-in [(first players) :role] "Spy")
                   (assoc-in [(first players) :actions] {:guess true :accuse true})))
             )
           )
     ;; Apply the list of functions to the playermap and return it
     playerMap)))

(defn- assign-roles
  "Creates one spy role, and fills the rest with a random place from gamedata"
  [gameMap]
  (let [location (rand-nth (keys gameData))
        ;; Roles do not include whoever is the spy
        roles (shuffle (-> gameData location :roles))
        ]
    (-> gameMap
        (assoc :location location)
        (update-in [:players] give-roles roles)
        )
    )
  )

(defn- create-start-message
  "Given a player and a location, creates a private message for
  each player. Checks for spy."
  [[playerName playerMap] location]
  (log/trace "Creating start message for player" playerName)
  (if (= "Spy" (:role playerMap))
    {:channel playerName
     :message (str "You are the spy! You must attempt to uncover the location.\n"
                   "You can guess secretly by using `,#channel guess location.\n"
                   "Possible locations: "
                   (apply str
                          (interpose ", "
                                     (map (fn [[_ {n :name}]] n) gameData)
                                     )
                          )
                   )
     }
    {:channel playerName
     :message (str "You are the "
                   (:role playerMap)
                   " in the "
                   (get-in gameData [location :name])
                   ".")}
    )
  )

(defn- send-start-messages
  "Creates messages for each of the players"
  [{players :players :as gameMap}]
  (assoc gameMap :message
         (conj
           ;; Individual player messsages
           (map create-start-message
                players
                (repeat (:location gameMap))
                )
           {:message (str "Guess who the spy is!\n"
                          "One person is a spy, and does not know where the rest of you are.\n"
                          (rand-nth (keys players)) " will start by asking any player a question.\n"
                          "After responding, that player will then ask any player a question, except the one who just asked them.\n"
                          "At any time, a player can `,accuse` another player of being a spy. The accusation only goes ahead if everyone but the target agrees. Each player can only make one accusation a game. Each player can `,agree` or `,disagree` with the vote.\n"
                          "The spy can `,guess` the location at any time, except during an accusation. If the spy guesses correctly, they win. Otherwise, everyone else wins."
                          )
            }
           )
         )
  )

(defn start
  "Starts the game if there's enough players"
  [gameMap commands {user :user channel :channel :as metaData}]
  (log/trace "Attempting to start game")
  (if (>= (count (:players gameMap)) 3)
    (-> gameMap
        assign-roles
        send-start-messages
        (assoc :status :playing)
        )
    ;; Not enough players
    (assoc gameMap :message (str "Not enough players. Current players: " (count (:players gameMap))))
    ))

(defn- score-game
  "Given a gameMap, a spy's name, and a boolean on whether the spy won, score the game"
  [gameMap spy spyWin?]
  (log/trace "Scoring game. Spy" spy (if spyWin? "won." "lost."))
  (let [players (set (keys (:players gameMap)))]
    (helpers/score-game
      (if spyWin?
        ;; Spy wins alone
        [spy]
        ;; Everyone else wins
        (disj players spy))
      players)))

(defn guess
  "Allows the spy to guess the correct location"
  [gameMap commands {user :user channel :channel :as metaData}]
  (log/trace "Player" user "is guessing" (first commands))
  (if (:accusing gameMap)
    ;; There's an accusation underway
    (assoc gameMap :message {:channel user :message "Can't guess during an accusation."})
    ;; No accusation underway
    (if (get-in gameMap [:players user :actions :guess])
      ;; This player can guess the location
      (if ((set (map :name (vals gameData)))
           (first commands))
        ;; This is a valid guess, maybe not correct though
        (if (= (clojure.string/lower-case (get-in gameData [(:location gameMap) :name]))
               (clojure.string/lower-case (first commands)))
          ;; Correct guess!
          (let [out (-> gameMap
                        (assoc :message (str "The spy "
                                             user
                                             " correctly guessed you were at the "
                                             (first commands)))
                        (assoc :status :ended)
                        (helpers/t-trace "Correct guess")
                        )]
            (score-game out user true)
            out
            )
          ;; Incorrect guess
          (let [out (-> gameMap
                        (assoc :message {:message (str "The spy "
                                                       user
                                                       " failed to guess you were at the "
                                                       (get-in gameData [(:location gameMap) :name])
                                                       " and instead thought you were at the "
                                                       (first commands))})
                        (assoc :status :ended)
                        (helpers/t-trace "Incorrect guess"))]
            (score-game out user false)
            out
            )
          )
        ;; Invalid guess
        (assoc gameMap :message {:message (str "Invalid guess.") :channel user}))
      ;; This player can't guess the location
      (assoc gameMap :message {:message (str "You are not allowed to guess the location") :channel user})
      )
    )
  )

(defn- get-spy
  "Gets the name of the spy in a gamemap"
  [{players :players :as gameMap}]
  (log/trace "get-spy. Players:" players)
  (key (first (filter (fn [[_ v]]
                        (log/trace "agree inner function value is:" v)
                        (= "Spy" (get v :role)))
                      players
                      )
              )
       )
  )

;; TODO restructure this mess to make it easier to track
(defn agree
  "Allows a player to agree a player should be picked as the spy."
  [{{target :target} :accusing :as gameMap} commands {user :user channel :channel :as metaData}]
  (log/trace "Agree. target:" target)
  (let [outMap (update-in gameMap
                          [:accusing :remaining]
                          disj
                          user)]
    (cond
      ;; Player can't agree
      (not (get-in gameMap [:accusing :remaining user]))
      (assoc gameMap :message {:channel user
                               :message "You are unable to agree"})
      ;; Not everyone has agreed the target is a spy
      (not (= 0 (count (get-in outMap [:accusing :remaining]))))
      (assoc outMap :message
             (str user " agreed that " target " is the spy."))
      ;; Everyone has agreed that the target is a spy
      ;; The target is a spy! Players win
      (= "Spy" (get-in outMap [:players target :role]))
      (let [out (-> outMap
                    (assoc :status :ended)
                    (helpers/assoc-message "The spy was caught!")
                    (helpers/assoc-message (str "The location was: " (get-in gameData [(:location gameMap) :name])))
                    )]
        (score-game out target false)
        out)
      ;; The target was not the spy. Good guys lose
      :failure
      (let [spy (get-spy outMap)
            out (-> outMap
                    (assoc :status :ended)
                    (helpers/assoc-message (str "The spy was actually " spy))
                    (helpers/assoc-message (str "The location was: " (get-in gameData [(:location gameMap) :name])))
                    )]
        (log/trace "Found spy as:" spy)
        (score-game out spy true)
        out)
      )
    )
  )

(defn disagree
  "Lets a player disagree with the current choice"
  [gameMap commands {user :user channel :channel :as metaData}]
  (if (get-in gameMap [:accusing :remaining user])
    ;; This player has yet to agree or disagree and is valid
    (-> gameMap
        (assoc-in [:players
                   (get-in gameMap [:accusing :accuser])
                   :actions
                   :accuse]
                  false)
        (dissoc :accusing)
        (assoc :message (str user " disagreed."))
        )
    (assoc gameMap :message {:channel user
                             :message "You are unable to disagree"})
    )
  )

(defn accuse
  "Lets a player attempt to accuse another player"
  [gameMap [target & _] {user :user channel :channel :as metaData}]
  (let [players (-> gameMap :players keys set)]
    (cond
      ;; Target doesn't start with @, add it
      (not (= (first target) \@))
      (recur gameMap [(str \@ target)] metaData)
      ;; Player can't accuse
      (not (get-in gameMap [:players user :actions :accuse]))
      (assoc gameMap :message "You are unable to accuse anyone")
      ;; Target is not in the list of players
      (not (players target))
      (assoc gameMap :message "Invalid target to accuse")
      ;; All is well, accusation can go ahead
      :can-accuse
      (-> gameMap
          ;; Set up the accusation
          (assoc :accusing
                 {:target target
                  :remaining (disj players user target)
                  :accuser user})
          ;; Tell everyone it's going ahead
          (assoc :message (str user
                               " has accused "
                               target
                               " of being the spy. agree or disagree."))
          )
      )
    )
  )

;; Admin
(defn debug
  "Sends the current gamemap to logs. Does nothing in-game"
  [{players :players :as gameMap} commands {user :user channel :channel :as metaData}]
  (log/info "debug. gameMap:" (pr-str gameMap))
  (log/info "commands:" commands)
  (log/info "metaData:" metaData)
  (assoc gameMap :message "Sent current gamemap to logs")
  )
