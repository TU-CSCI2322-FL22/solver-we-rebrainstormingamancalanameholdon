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
bestMove gs = undefined
--    let outcomes = [(whoWillWin (checkMove gs move), move) | move <- (validMoves gs)]
--    in ()

findWinMove :: [(Outcome, Move)] -> Player -> Maybe Move
findWinMove [] player = Nothing
findWinMove ((o,m):tups) player = if o == Win player then m else findWinningMove tups

findTieMove :: [(Outcome, Move)] -> Player -> Maybe Move
findTieMove = undefined


    


    


--
-- write a "best move"
-- Possible type:
-- bestMove :: GameState -> Move




