
-- Board Representation Aliases
--

type Store = Int
type Hole = (Int, Int) -- (hole #, # of beans)

-- Board and Game State Algebraic Data Types
--

data Board = Board { store1 :: Store,
                     holes1 :: [Hole],
                     store2 :: Store,
                     holes2 :: [Hole] } deriving (Show) 
                    
data Player = Player1 | Player2 deriving Show -- NOTE: GameOver may ultimately be unnecessary

data Outcome = Win Player | Tie | NotOver deriving Show

-- Gamestate and Gameplay (i.e., Move, ...) Aliases
--

type GameState = (Player, Board)
type Move = Int -- NOTE: this alias is currently subject to change

-- Helper Variables
--

-- startState is a State, and should be the very first state that a player sees at the beginning of
-- a new game. 
-- Once startBoard is defined, startState should be defined as (Player1, startBoard).
-- If the combination of startBoard and startState is confusing, we can change it later.
startState :: GameState
startState = (Player1, Board 0 [(1,4),(2,4),(3,4),(4,4),(5,4),(6,4)] 0 [(7,4),(8,4),(9,4),(10,4),(11,4),(12,4)])

-- Function Stubs
--

-- isValid is a function that should determine if the desired move is valid.
-- For a move to be valid, the chosen hole number must be a valid hole number. If the hole number is
-- valid, we must then ensure that the hole contains at least one bead to move.
-- If the current player is Player1, they must choose a hole number from 1-6. If the current player
-- is Player2, they must choose a hole number from 7-12.
-- If you feel the need to change the type signature, please do so... but LET THE GROUP KNOW FIRST
-- and BE AWARE OF THE POTENTIAL REPERCUSSIONS OF DOING SO.

-- Jeremy and David:
isValid :: Move -> GameState -> Bool
isValid move state = 
    let
        moves = validMoves state
    in move `elem` moves 
        

{- validMoves takes a GameState and returns a list of moves.
    If the player cannot choose the hole because it is not on their side, it is an invalid move--exclude it from the list.
    If the hole is empty, it is an invalid move--exclude it from the list.
    Return the list of moves that have at least 1 stone and are on the player's side.
-}

-- Jeremy and David:
validMoves :: GameState -> [Move]
validMoves state@(turn, board) = 
    let
        moves = case turn of Player1 -> (holes1 board); Player2 -> (holes2 board)
    in [fst x | x <- moves, (snd x) /= 0]

-- isOver is a function that should determine if the game has ended. For a game to be over, the
-- holes on one side of the board (or the other) should have NO BEANS in them. I recommend that you
-- use the higher-order function all in your implementation. 
-- If you feel the need to change the type signature, please do so... but LET THE GROUP KNOW FIRST
-- and BE AWARE OF THE POTENTIAL REPERCUSSIONS OF DOING SO.

-- Leanna and Michelle:
emptyHoles :: [Hole] -> Bool
emptyHoles holes = all (\(loc,beans) -> beans == 0) holes

isOver :: Board -> Bool
isOver board = emptyHoles (holes1 board) || emptyHoles (holes2 board)

-- makeMove is a function that should take in some representation of a move and current game state
-- and return a game state with the appropriate changes to the board / turn having been made.
-- If you feel the need to change the type signature, please do so... but LET THE GROUP KNOW FIRST
-- and BE AWARE OF THE POTENTIAL REPERCUSSIONS OF DOING SO.

-- Leanna and Michelle:
makeMove :: Move -> GameState -> Maybe GameState
makeMove = undefined

-- getWinner is a function that should take in a board or game state and use that board or game
-- state to determine the winner of the game. This function should only be successfully called after a game is
-- over. There is no type signature for this function yet, but it should look something like the
-- commented out signatures below.

-- Leanna and Michelle:
getOutcome :: GameState -> Outcome
getOutcome (player, board)
    | not (isOver board) = NotOver
    | store1 board == store2 board = Tie
    | store1 board > store2 board = Win Player1
    | store1 board < store2 board = Win Player2

-- showGame is a function that should take a game state and return somethat that will show all the information that a user
-- might want to see in a legible way (i.e., a list of strings with the current turn, the number of
-- beans in holes, and the number of beans in stores and (perhaps) some labels for these holes and
-- stores).
-- If you feel the need to change the type signature, please do so... but LET THE GROUP KNOW FIRST
-- and BE AWARE OF THE POTENTIAL REPERCUSSIONS OF DOING SO.


-- Jeremy and David:
showGame :: GameState -> String
showGame state@(turn, Board s1 h1 s2 h2) = 
    let
        holesToStr :: [Hole] -> String
        -- holesToStr [x] = (show (snd x)) ++ "   "
        -- holesToStr (x:xs) = concat [holesToStr xs,show (snd x), "   "]
        holesToStr y@(x:xs) =
            let
                spaces = if (snd x) > 9 then "  " else "   "
            in case y of [x] -> (show (snd x)) ++ spaces
                         (x:xs) -> concat [holesToStr xs,show (snd x), spaces]

        labelsToStr :: [Hole] -> String
        labelsToStr y @(x:xs) =
            let
                spaces = if (snd x) > 9 then "  " else "   "
            in case y of [x] -> (show (fst x)) ++ spaces
                         (x:xs) -> concat [labelsToStr xs, show (fst x), spaces]
        spaces = if s2 > 9 then "" else "  "
        newH2 = concat ["    12  11  10  9   8   7\n", (show s2), spaces, "| ",  (holesToStr h2)] -- "    12   11   10   9   8   7\n"
        newH1 = concat [(holesToStr (reverse h1)), " | ", (show s1), "\n", "  ", "  1   2   3   4   5   6\n"] --reverse (labelsToStr h1)] -- "    1   2   3   4  5   6\n"
        turnStr = case turn of  Player1 -> "Player 1"
                                Player2 -> "Player 2"
        outcome = case getOutcome state of  Win Player1 -> "Player 1 won!"
                                            Win Player2 -> "Player 2 won!"
                                            Tie -> "It's a tie!"
                                            NotOver -> "Game in progress. It is " ++ turnStr ++ "'s turn."

    in concat ["\n", outcome, "\n", newH2, "\n    ", newH1]
 

-- FULL CREDIT: We need to change these functions (including their type signatures, as necessary) to consider ALL
-- possible errors or edge cases. We will likely need to use Maybe and Either. Note that this WILL
-- break things and we WILL need to be diligent about communicating with one another.
--
-- We must consider:
-- What will happen if there is a draw?
-- What will happen if a move is invalid?
-- Others...?
-- STEALSIn the State Managers worksheet, insert the TEXTJOIN function in cell D2 to use a space as the delimiter to join the text in the range A2:C2. Set the Ignore_empty argument to TRUE. 
