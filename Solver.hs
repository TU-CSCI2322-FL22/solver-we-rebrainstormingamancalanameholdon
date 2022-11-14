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
        Nothing -> findBestOutcome [whoWillWin (checkMove gs move) | move <- (validMoves gs)] player
        Just winner -> winner

-- findBestGS :: GameState -> [Move] -> GameState
-- findBestGS gs@(player, board) moves = undefined
findBestOutcome :: [Outcome] -> Player -> Outcome
findBestOutcome outcomes player
    | Win player `elem` outcomes = Win player
    | Tie `elem` outcomes = Tie
    | otherwise = Win (if player == Player1 then Player2 else Player1)



checkMove :: GameState -> Move -> GameState
checkMove gs move = 
    case (makeMove move gs) of
         Nothing -> error ("Move was invalid: " ++ (show move))
         Just newGS -> newGS

bestMove :: GameState -> Move
bestMove gs@(player, board) =
    let outcomes = [(whoWillWin (checkMove gs move), move) | move <- (validMoves gs)]
    in  case findWinMove outcomes player of
             Just move -> move
             Nothing -> case findTieMove outcomes of
                             Just move -> move
                             Nothing -> case findOtherMove outcomes of
                                             Just move -> move
                                             Nothing -> error ("No valid moves for gs in bestMove: " ++ (show outcomes))

findWinMove :: [(Outcome, Move)] -> Player -> Maybe Move
findWinMove [] player = Nothing
findWinMove ((o,m):tups) player = if o == Win player then (Just m) else findWinMove tups player

findTieMove :: [(Outcome, Move)] -> Maybe Move
findTieMove [] = Nothing
findTieMove ((o,m):tups) = if o == Tie then (Just m) else findTieMove tups

findOtherMove :: [(Outcome, Move)] -> Maybe Move
findOtherMove [] = Nothing
findOtherMove ((o,m):tups) = Just m


    


    


--
-- write a "best move"
-- Possible type:
-- bestMove :: GameState -> Move




