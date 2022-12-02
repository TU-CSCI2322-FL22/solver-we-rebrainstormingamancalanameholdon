module Solver where

import Mancala
-- module things
--
-- determine optimal move for a player for a game state
--
-- search for a move that can force a win for current player
--
-- return move that can force a tie for the current player when a win is not possible
--
--
-- talk to Fogarty
-- think about what the heck you need to do
--
-- refactor outcome type in Mancala.hs
--
-- have current getOutcome return Maybe Outcome
--
-- write a "who will win" function
-- Possible type:
--

whoWillWin :: GameState -> Outcome
whoWillWin gs@(player, board) = 
    case getOutcome gs of
        Nothing ->  let newGSs = catMaybe [makeMove gs move | move <- (validMoves gs)]
                    in findBestOutcome [whoWillWin newGS | newGS <- newGSs] player                        
        Just winner -> winner

-- findBestGS :: GameState -> [Move] -> GameState
-- findBestGS gs@(player, board) moves = undefined
findBestOutcome :: [Outcome] -> Player -> Outcome
findBestOutcome outcomes player
    | Win player `elem` outcomes = Win player
    | Tie `elem` outcomes = Tie
    | otherwise = Win (if player == Player1 then Player2 else Player1)

-- look for other places we use checkMove and fix?
-- ask why this is unsafe?
checkMove :: GameState -> Move -> GameState
checkMove gs move = 
    case (makeMove move gs) of
         Nothing -> error ("Move was invalid: " ++ (show move))
         Just newGS -> newGS


bestMove :: GameState -> Maybe Move
bestMove gs@(player, board) =
    let outcomes = [(whoWillWin (checkMove gs move), move) | move <- (validMoves gs)]
    in  case findWinMove outcomes player of
             Just move -> Just move
             Nothing -> case findTieMove outcomes of
                             Just move -> Just move
                             Nothing -> case findOtherMove outcomes of
                                             Just move -> Just move
                                             Nothing -> Nothing

findWinMove :: [(Outcome, Move)] -> Player -> Maybe Move
findWinMove [] player = Nothing
findWinMove ((o,m):tups) player = if o == Win player then (Just m) else findWinMove tups player

findTieMove :: [(Outcome, Move)] -> Maybe Move
findTieMove [] = Nothing
findTieMove ((o,m):tups) = if o == Tie then (Just m) else findTieMove tups

findOtherMove :: [(Outcome, Move)] -> Maybe Move
findOtherMove [] = Nothing
findOtherMove ((o,m):tups) = Just m

evalHoles :: Player -> Move -> [Hole] -> Int
evalHoles player move [] = 0
evalHoles Player1 move ((loc,beans):holes)
    | beans == 0 = evalHoles Player1 (move+1) holes + 1
    | beans == 7-move = evalHoles Player1 (move+1) holes + beans + 2
    | beans > 7-move = evalHoles Player1 (move+1) holes + 7 - move
    | beans < 7-move = evalHoles Player1 (move+1) holes + beans
evalHoles Player2 move ((loc,beans):holes)
    | beans == 0 = evalHoles Player2 (move+1) holes + 1
    | beans == 13-move = evalHoles Player2 (move+1) holes + beans + 2
    | beans > 13-move = evalHoles Player2 (move+1) holes + 13 - move
    | beans < 13-move = evalHoles Player2 (move+1) holes + beans

-- fast and dumb!!!
evalSide :: Player -> GameState -> Int
-- store + bean potential
evalSide Player1 (player, Board s1 h1 s2 h2) = s1 + evalHoles Player1 1 h1 + (if player == Player1 then 1 else 0)
evalSide Player2 (player, Board s1 h1 s2 h2) = s2 + evalHoles Player2 7 h2 + (if player == Player2 then 1 else 0)

evalGame :: GameState -> Int
evalGame gs =
    case getOutcome gs of
         Just (Win Player1) -> 999
         Just (Win Player2) -> -999
         Just Tie -> 0
         Nothing -> evalSide Player1 gs - evalSide Player2 gs
-- add/subtract points depending on whose turn it is currently

findGoodOutcome :: Player -> [Int] -> Int
findGoodOutcome Player1 = maximum
findGoodOutcome Player2 = minimum

whoMightWin :: GameState -> Int -> Int
whoMightWin gs@(player,board) depth
    | depth == 0 || isOver board = evalGame gs
    | otherwise = findGoodOutcome player [whoMightWin (checkMove gs move) (depth-1) | move <- validMoves gs] 

findGoodMove :: Player -> [(Int,Move)] -> Move
findGoodMove Player1 tups = snd $ maximum tups
findGoodMove Player2 tups = snd $ minimum tups

goodMove :: GameState -> Int -> Move
goodMove gs@(player, board) depth = findGoodMove player [(whoMightWin (checkMove gs move) depth, move) | move <- validMoves gs]

{-
--int is depth (how many moves ahead we can look if necesary)
goodMove :: GameState -> Int -> Move
-}
