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

