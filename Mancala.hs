--module Mancala where
import Data.List
import Data.Maybe
import Debug.Trace

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

loadedboard = (Player1, Board 22 [(1,11),(2,11),(3,11),(4,11),(5,11),(6,11)] 22 [(7,22),(8,22),(9,22),(10,22),(11,22),(12,22)])
deadboard = (Player1, Board 0 [(1,0),(2,0),(3,0),(4,0),(5,0),(6,0)] 0 [(7,2000000),(8,0),(9,0),(10,0),(11,0),(12,0)])
identityboard = (Player1, Board 22 [(1,1),(2,2),(3,3),(4,4),(5,5),(6,6)] 22 [(7,7),(8,8),(9,9),(10,10),(11,11),(12,12)])

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

emptyHole :: Int -> [Hole] -> (Int, [Hole])
emptyHole index holes = case splitAt index holes of
                             (leftOf, ((loc,beans):rightOf)) -> (beans, leftOf ++ [(loc,0)] ++ rightOf)
                             _ -> error ("splitAt not working in emptyHole: " ++ show (splitAt index holes))

takeBeans :: Move -> GameState -> (Int, GameState)
takeBeans move gamestate@(player, Board s1 h1 s2 h2) = 
    let (held, newHoles) = if move < 7 then emptyHole (move-1) h1 else emptyHole (move-7) h2
    in  (held, updatePlayerSide gamestate newHoles) 

dropInSide :: Move -> Int -> [Hole] -> (Int,[Hole])
dropInSide start held holes =
    let (leftOf, rightOf) = splitAt (start-1) holes
        (dropIn, noDropIn) = splitAt held rightOf
    in  (held-(length dropIn), leftOf ++ [(loc,beans+1) | (loc,beans) <- dropIn] ++ noDropIn)

droppedInEmpty :: Move -> Int -> [Hole] -> Bool
droppedInEmpty move held holes = 
    let moveIndex = if move<7 then move else move-6
    in  case splitAt (moveIndex+held-2) holes of
             (leftOf,((loc,beans):rightOf)) -> beans == 1
             _ -> error ("splitAt not working in checkOppHole: " ++ show (splitAt (moveIndex+held-2) holes))

checkOppHole :: Move -> Int -> [Hole] -> Bool
checkOppHole move held holes = 
    let moveIndex = if move<7 then move else move-6
    in  case splitAt (5-(moveIndex+held-2)) holes of
             (leftOf,((loc,beans):rightOf)) -> beans /= 0
             _ -> error ("splitAt not working in checkOppHole: " ++ show (splitAt (5-(moveIndex+held-2)) holes))

dropBeans :: Move -> Int -> GameState -> GameState
dropBeans move held gamestate@(player, Board s1 h1 s2 h2) = 
    let (holes, store) = getPlayerSide gamestate
        (holesOpp, storeOpp) = getOppSide gamestate
        (leftOver, newSide) = dropInSide move held holes
        newGameState = updatePlayerSide gamestate newSide
    in  case leftOver of 
        0 -> if (droppedInEmpty move held newSide) && (checkOppHole move held holesOpp)
             then let (beansP, newPlayerHoles) = emptyHole (move+held-2) newSide
                      newPlayerSide = updatePlayerSide newGameState newPlayerHoles
                      (beansO, newOppHoles) = emptyHole (5-(move+held-2)) holesOpp
                      newOppSide = updateOppSide newPlayerSide newOppHoles
                  in  switchTurn (updatePlayerStore newOppSide (store+beansP+beansO))
             else switchTurn newGameState
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
showOutcome :: GameState -> String
showOutcome state@(turn, Board x y z a) = 
    let turnStr = case turn of  Player1 -> "Player 1"
                                Player2 -> "Player 2"
    in case getOutcome state of Win Player1 -> "Player 1 won!"
                                Win Player2 -> "Player 2 won!"
                                Tie -> "It's a tie!"
                                NotOver -> "Game in progress. It is " ++ turnStr ++ "'s turn."



showGame :: GameState -> String
showGame state@(turn, Board s1 h1 s2 h2) = 
    let showHoles y@(x:xs) = concat (map (\x -> show (snd x) ++ if (snd x) > 9 then "  " else "   ") y)
        spaces = if s1 > 9 then " " else "  "
        newH2 = concat ["      12  11  10  9   8   7\n", (show s2), spaces, "|","  ",  (showHoles (reverse h2))] 
        newH1 = concat ["      ", (showHoles (h1)), "| ", (show s1), "\n", "      1   2   3   4   5   6"] 
    in unlines [showOutcome state, newH2, newH1]
    --in concat ["\n", outcome, "\n", newH2, "\n    ", newH1]

-- FULL CREDIT: We need to change these functions (including their type signatures, as necessary) to consider ALL
-- possible errors or edge cases. We will likely need to use Maybe and Either. Note that this WILL
-- break things and we WILL need to be diligent about communicating with one another.
--
-- We must consider:
-- What will happen if there is a draw?
-- What will happen if a move is invalid?
-- Others...?
-- STEALSIn the State Managers worksheet, insert the TEXTJOIN function in cell D2 to use a space as the delimiter to join the text in the range A2:C2. Set the Ignore_empty argument to TRUE. 
