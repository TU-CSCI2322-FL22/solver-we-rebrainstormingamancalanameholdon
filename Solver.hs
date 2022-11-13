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

{-whoWillWin gs = 
    case validMoves gs of 
        [] -> case getOutcome gs of
                    Nothing -> error ("Error determining winner given gamestate: " ++ (showGame gs))
                    Just winner -> winner 
        [move] -> whoWillWin (checkMove gs move)
        --(x:xs) -> -}

-- findBestGS :: GameState -> [Move] -> GameState
-- findBestGS gs@(player, board) moves = undefined

findBestOutcome :: [Outcome] -> Player -> Outcome
findBestOutcome outcomes player
    | Win player `elem` outcomes = Win player
    | Tie `elem` outcomes = Tie
    | otherwise = Win (if player == Player1 then Player2 else Player1)

--snd (max [ (player Store, gs) | ]) --newGS = (checkMove gs move)
    {-let newStates = [(if player == Player1 then store1 (snd (checkMove gs move)) else store2 (snd (checkMove gs move)), (checkMove gs move)) | move <- moves]
        (_,b) = foldr1 (\a b -> if fst a > fst b then a else b) newStates 
    in  b -}

checkMove :: GameState -> Move -> GameState
checkMove gs move = 
    case (makeMove move gs) of
         Nothing -> error ("Move was invalid: " ++ (show move))
         Just newGS -> newGS


    


--
-- write a "best move"
-- Possible type:
-- bestMove :: GameState -> Move




