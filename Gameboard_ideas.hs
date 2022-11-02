import Debug.Trace
import Data.Maybe
--import Data.Record

data Board = Board  {store1 :: Int
                    , holes1 :: [Hole]
                    , store2 :: Int
                    , holes2 :: [Hole]} deriving Show
{-
Store1 Store 
Holes1 Holes 
Store2 Store  
Holes2 Holes 
-}

type Store = Int
type Hole = (Int, Int)
--type Move = undefined
type End = Bool
--type Turn = Bool
data Turn = Player1 | Player2 | GameOver

type GameState = (Turn, Board)
--startState = 

-- validHoles

--checkEnd :: Board -> Bool -- Check if the game is over

-- isValidMove 

-- move :: GameState -> GameState

-- winner :: GameState -> 


-- instance Show GameState where
