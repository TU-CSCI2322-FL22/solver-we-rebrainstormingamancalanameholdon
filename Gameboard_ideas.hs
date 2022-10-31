import Debug.Trace
import Data.Maybe
--import Data.Record

data Board = Board  {store1 :: Int
                    , holes1 :: [Int]
                    , store2 :: Int
                    , holes2 :: Int} deriving Show
{-
Store1 Store 
Holes1 Holes 
Store2 Store  
Holes2 Holes 
-}

type Store = Int
type Holes = [Int]
--type Move = undefined
type End = Bool
--type Turn = Bool
-- data Turn = Player1 | Player2 | Endgame

checkEnd :: Board -> Bool
checkEnd board = 
    let
        aux lst = 
            let nonEmpties = [x | x <- lst, x /= []]
            in nonEmpties != []
    in aux holes1 || aux holes2
