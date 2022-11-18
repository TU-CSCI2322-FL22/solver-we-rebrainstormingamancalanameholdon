module Main where

import Mancala
import Solver
--import Data.List.Split
import Data.List.Extra
import Data.Maybe
import Text.Read
import Data.Char

-- Note that we could add a 25-bean win condition to isOver to optimize?

-- Milestone 3: 
-- 1) write main that returns best move a player can make given a gs
-- 2) write a function that returns an integer value that tells you who is doing better (positive
-- values for player 1, negative values for player 2) given a gs

-- be wary of weights

-- other function ideas: (gamestate status ideas)
-- 
-- -> add/subtract (depending on side) the number of holes that will end on your side, if you make
-- that move
-- -> store 1 - store 2
--

main :: IO ()
main = do
    putStrLn "Hello, World!"
    -- take in filename
    -- read contents of file
    -- turn contents into a gamestate
    -- call bestMove if we have a valid input gamestate (in-progress, not nothing)
    -- print out the result of bestMove to stdout

--Will return nothing if we have an extra space...
getPlayer :: String -> Maybe Player
getPlayer "1" = Just Player1
getPlayer "2" = Just Player2
getPlayer _ = Nothing

getHoles :: String -> [Int] -> Maybe [Hole]
getHoles str labels = 
    let stringHoles = splitOn " " str
        filteredHoles = filter (\hole -> hole /= "" && all (isDigit) hole) stringHoles
        numBeans = map (\stringBeans -> read stringBeans :: Int) filteredHoles
        posBeans = filter (>= 0) numBeans
    in  case length posBeans of 
             6 -> Just $ zip labels posBeans
             _ -> Nothing
        
getStore :: String -> Maybe Store
getStore str =
    let store = (readMaybe str :: Maybe Int)
    in  if store /= Nothing && store >= Just 0
        then store
        else Nothing

readGame :: String -> Maybe GameState
readGame inputGS =
    let newInputGS = trim inputGS 
    in  case lines newInputGS of
             playerLine:s1Line:h1Line:s2Line:h2Line:[] -> do
                player <- getPlayer (trim playerLine)
                s1 <- getStore s1Line
                h1 <- getHoles h1Line [1..6]
                s2 <- getStore s2Line
                h2 <- getHoles h2Line [7..12]
                return (player, Board s1 h1 s2 h2)
             _ -> Nothing

uglyShowPlayer :: Player -> String
uglyShowPlayer Player1 = "1"
uglyShowPlayer Player2 = "2"

uglyShowHoles :: [Hole] -> String
uglyShowHoles holes = concat $ map (\(loc,beans) -> show beans ++ " ") holes

uglyShowGame :: GameState -> String
uglyShowGame gs@(player, Board s1 h1 s2 h2) =
    let uglyPlayer = uglyShowPlayer player
        uglyS1 = show s1
        uglyH1 = uglyShowHoles h1
        uglyS2 = show s2
        uglyH2 = uglyShowHoles h2
    in  unlines [uglyPlayer,uglyS1,uglyH1,uglyS2,uglyH2]

writeGame :: GameState -> FilePath -> IO ()
writeGame gs file = do
    writeFile file (uglyShowGame gs)

loadGame :: FilePath -> IO (Maybe GameState)
loadGame file = do
    contents <- readFile file
    let gs = do readGame contents
    return gs

putWinner :: GameState -> IO ()
putWinner gs = do
    let winner = whoWillWin gs
    putStrLn (show winner)

