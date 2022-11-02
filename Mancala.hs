
-- Board Representation Aliases
--

type Store = Int
type Hole = (Int, Int)

-- Board and Game State Algebraic Data Types
--

data Board = Board { store1 :: Store,
                     holes :: [Hole],
                     store2 :: Store,
                    } deriving (Show) 

data Player = Player1 | Player2 deriving Show -- NOTE: GameOver may ultimately be unnecessary

data Outcome = Win Player | Tie | NotOver deriving Show

-- Gamestate and Gameplay (i.e., Move, ...) Aliases
--

type GameState = (Player, Board)
type Move = Int -- NOTE: this alias is currently subject to change

-- Helper Variables
--

-- validHoleNums is a list of all the possible integer values that can be associated with a given hole.
-- validHoleNums should be used when checking if a move is valid.
validHoleNums :: [Int]
validHoleNums = [1..6] -- may want to have 1..12 instead

-- startBoard is a Board that should look exactly like a Mancala board does at the start of a new
-- game.
startBoard :: Board
startBoard = undefined

-- startState is a State, and should be the very first state that a player sees at the beginning of
-- a new game. 
-- Once startBoard is defined, startState should be defined as (Player1, startBoard).
-- If the combination of startBoard and startState is confusing, we can change it later.
startState :: GameState
startState = undefined

-- Function Stubs
--

-- isValid is a function that should determine if the desired move is valid.
-- For a move to be valid, the chosen hole number must be a valid hole number. If the hole number is
-- valid, we must then ensure that the hole contains at least one bead to move.
-- If you feel the need to change the type signature, please do so... but LET THE GROUP KNOW FIRST
-- and BE AWARE OF THE POTENTIAL REPERCUSSIONS OF DOING SO.
isValid :: Move -> GameState -> Bool
isValid = undefined

validMoves :: GameState -> [Move]
validMoves = undefined

-- isOver is a function that should determine if the game has ended. For a game to be over, the
-- holes on one side of the board (or the other) should have NO BEANS in them. I recommend that you
-- use the higher-order function all in your implementation. 
-- If you feel the need to change the type signature, please do so... but LET THE GROUP KNOW FIRST
-- and BE AWARE OF THE POTENTIAL REPERCUSSIONS OF DOING SO.
isOver :: Board -> Bool
isOver = undefined

-- makeMove is a function that should take in some representation of a move and current game state
-- and return a game state with the appropriate changes to the board / turn having been made.
-- If you feel the need to change the type signature, please do so... but LET THE GROUP KNOW FIRST
-- and BE AWARE OF THE POTENTIAL REPERCUSSIONS OF DOING SO.
makeMove :: Move -> GameState -> Maybe GameState
makeMove = undefined

-- getWinner is a function that should take in a board or game state and use that board or game
-- state to determine the winner of the game. This function should only be successfully called after a game is
-- over. There is no type signature for this function yet, but it should look something like the
-- commented out signatures below.

getWinner :: GameState -> Outcome
getWinner = undefined

-- showGame is a function that should take a game state and return somethat that will show all the information that a user
-- might want to see in a legible way (i.e., a list of strings with the current turn, the number of
-- beans in holes, and the number of beans in stores and (perhaps) some labels for these holes and
-- stores).
-- If you feel the need to change the type signature, please do so... but LET THE GROUP KNOW FIRST
-- and BE AWARE OF THE POTENTIAL REPERCUSSIONS OF DOING SO.

showGame :: GameState -> String
showGame = undefined

-- FULL CREDIT: We need to change these functions (including their type signatures, as necessary) to consider ALL
-- possible errors or edge cases. We will likely need to use Maybe and Either. Note that this WILL
-- break things and we WILL need to be diligent about communicating with one another.
--
-- We must consider:
-- What will happen if there is a draw?
-- What will happen if a move is invalid?
-- Others...?
-- STEALS
