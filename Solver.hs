module Solver where

import Mancala
-- module things
--
--
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
whoWillWin :: GameState -> Outcome
whoWillWin gs = 
    case getOutcome gs of
        Nothing -> whoWillWin gs
        Just winner -> winner


--
-- write a "best move"
-- Possible type:
-- bestMove :: GameState -> Move




