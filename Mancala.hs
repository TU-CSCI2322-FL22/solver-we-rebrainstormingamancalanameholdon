import Data.List
import Data.Maybe

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
                    
data Player = Player1 | Player2 deriving (Show, Eq) -- NOTE: GameOver may ultimately be unnecessary

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

getPlayerSide :: GameState -> ([Hole],Store)
getPlayerSide (player, Board s1 h1 s2 h2) = if player == Player1 then (h1,s1) else (h2,s2)

getOppSide :: GameState -> ([Hole],Store)
getOppSide (player, Board s1 h1 s2 h2) = if player == Player2 then (h1,s1) else (h2,s2)

updatePlayerSide :: GameState -> [Hole] -> GameState
updatePlayerSide (player, Board s1 h1 s2 h2) holes 
    | player == Player1 = (player, Board s1 holes s2 h2)
    | player == Player2 = (player, Board s1 h1 s2 holes)

updatePlayerStore :: GameState -> Store -> GameState
updatePlayerStore (player, Board s1 h1 s2 h2) store = if player == Player1
                                                      then (player, Board store h1 s2 h2)
                                                      else (player, Board s1 h1 store h2)

updateOppSide :: GameState -> [Hole] -> GameState
updateOppSide (player, Board s1 h1 s2 h2) holes
    | player == Player2 = (player, Board s1 holes s2 h2)
    | player == Player1 = (player, Board s1 h1 s2 holes)

switchTurn :: GameState -> GameState
switchTurn (player, board) = if player == Player1 then (Player2, board) else (Player1, board)

-- takes move and gamestate and returns the number of held beans and the updated gamestate (set move to 0)
takeBeans :: Move -> GameState -> (Int, GameState)
takeBeans move gamestate@(player, Board s1 h1 s2 h2) = 
    let (leftOf,((loc,held):rightOf)) = if move < 7
                                        then splitAt (move-1) h1
                                        else splitAt (move-7) h2
        newChosen = (loc, 0)
        newHoles = leftOf ++ [newChosen] ++ rightOf
    in  (held, updatePlayerSide gamestate newHoles) 

dropInSide :: Move -> Int -> [Hole] -> (Int,[Hole])
dropInSide start held holes =
    let (leftOf, rightOf) = splitAt (start-1) holes
        (dropIn, noDropIn) = (take held rightOf, drop held rightOf)
    in  (held-(length dropIn), leftOf ++ [(loc,beans+1) | (loc,beans) <- dropIn] ++ noDropIn)

droppedInEmpty :: Move -> Int -> [Hole] -> Bool
droppedInEmpty move held holes = 
    let (leftOf,((loc,beans):rightOf)) = splitAt (move+held-1) holes
    in  beans == 1

checkOppHole :: Move -> Int -> [Hole] -> Bool
checkOppHole move held holes = 
    let (leftOf,((loc,beans):rightOf)) = splitAt (5-(move+held-1)) holes
    in  beans /= 0

dropBeans :: Move -> Int -> GameState -> GameState
dropBeans move held gamestate@(player, Board s1 h1 s2 h2) = 
    let (holes, store) = getPlayerSide gamestate
        (leftOver, newSide) = dropInSide move held holes
        newGameState = updatePlayerSide gamestate newSide
    in  case leftOver of 
        0 -> switchTurn newGameState
        1 -> updatePlayerStore newGameState (store+1)
        x -> let newNewGameState = updatePlayerStore newGameState (store+1) 
             in giveBeans (leftOver-1) newNewGameState

giveBeans :: Int -> GameState -> GameState
giveBeans held gamestate@(player, Board s1 h1 s2 h2) = 
    let (holes, store) = getOppSide gamestate
        (leftOver, newSide) = dropInSide 1 held holes
        newGameState = updateOppSide gamestate newSide
    in  case leftOver of
        0 -> switchTurn newGameState
        x -> dropBeans 1 leftOver newGameState

makeMove :: Move -> GameState -> Maybe GameState
makeMove move gamestate@(player, Board s1 h1 s2 h2)
    | isValid move gamestate = 
          let loc = if move >=7 && move <= 11 then move-6 else move
              (held, newGameState) = takeBeans move gamestate
          in  Just (dropBeans (loc+1) held newGameState)
    | otherwise = Nothing

-- getWinner is a function that should take in a board or game state and use that board or game
-- state to determine the winner of the game. This function should only be successfully called after a game is
-- over. There is no type signature for this function yet, but it should look something like the
-- commented out signatures below.

-- Leanna and Michelle:
getWinner :: GameState -> Outcome
getWinner = undefined

-- showGame is a function that should take a game state and return somethat that will show all the information that a user
-- might want to see in a legible way (i.e., a list of strings with the current turn, the number of
-- beans in holes, and the number of beans in stores and (perhaps) some labels for these holes and
-- stores).
-- If you feel the need to change the type signature, please do so... but LET THE GROUP KNOW FIRST
-- and BE AWARE OF THE POTENTIAL REPERCUSSIONS OF DOING SO.


-- Jeremy and David:
showGame :: GameState -> String
showGame state@(turn, board) = 
    let
        s1 = store1 board
        s2 = store2 board
        h1 = holes1 board
        h2 = holes2 board
        holesToStr :: [Hole] -> String -> String
        holesToStr [] acc = acc
        holesToStr [x] acc = (acc ++ "  " ++ (show (snd x)))
--holesToStr [x] acc = ((show (snd x)) ++ "  " ++ acc)
        holesToStr holes@(x:xs) acc = holesToStr xs (acc ++ "  " ++ (show (snd x)))
--holesToStr holes@(x:xs) acc = holesToStr xs ((show (snd x)) ++ "  " ++ acc)
        newH2 = "    12 11 10 9  8  7\n" ++ (show s2) ++ " | " ++ (reverse (holesToStr h2 []))
        newH1 = (holesToStr h1 []) ++ " | " ++ (show s1) ++ "\n" ++ "    1  2  3  4  5  6"
    in concat [newH2, "\n  ", newH1]
--        newH1 = "     1  2  3  4  5  6\n" ++ (show s1) ++ " |" ++ (reverse (holesToStr h1 []))
--        newH2 = (holesToStr h2 []) ++ "| " ++ (show s2) ++ "\n" ++ "     12 11 10 9  8  7\n"
--    in concat [newH1, "\n     ", newH2]
 

-- FULL CREDIT: We need to change these functions (including their type signatures, as necessary) to consider ALL
-- possible errors or edge cases. We will likely need to use Maybe and Either. Note that this WILL
-- break things and we WILL need to be diligent about communicating with one another.
--
-- We must consider:
-- What will happen if there is a draw?
-- What will happen if a move is invalid?
-- Others...?
-- STEALS
