module Main where

import Mancala
import Solver
import Data.List.Split

main :: IO ()
main = do
    putStrLn "Hello, World!"
    --readGame "1\n0\n4 4 4 4 4 4\n0\n4 4 4 4 4 4"


-- module things

-- read states from a file
--
-- write game states to a file
--
-- compute the winning move
--
-- print the winning move
--
--
--Will return nothing if we have an extra space...
getPlayer :: String -> Maybe Player
getPlayer "1" = Just Player1
getPlayer "2" = Just Player2
getPlayer _ = Nothing

getHoles :: String -> [Int] -> Maybe [Hole]
getHoles str labels = 
    let stringHoles = splitOn " " str
        filteredHoles = filter (/= "") stringHoles
        numBeans = map (\stringBeans -> read stringBeans :: Int) filteredHoles
        posBeans = filter (>= 0) numBeans
    in  case length posBeans of 
             6 -> Just $ zip labels posBeans
             _ -> Nothing
        
getStore :: String -> Store
getStore str = (read str :: Store)

-- Potentially utilize strip/trim on inputs to helper functions and lines.
{-
readGame :: String -> Maybe GameState
readGame inputGS =  
    case lines inputGS of
        playerLine:s1Line:h1Line:s2Line:h2Line:[] -> Just $ (getPlayer playerLine, Board (getStore s1Line) (getHoles h1Line [1..6]) (getStore s2Line) (getHoles h2Line [7..12]))
        _ -> Nothing
-}
--change to take a list of strings as input for helper functions

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

-- import from Mancala module

writeGame :: GameState -> FilePath -> IO ()
writeGame gs file = do
    writeFile file (uglyShowGame gs)
{-
loadGame :: FilePath -> IO (Maybe GameState)
loadGame file = do
    contents <- readFile file
    let gs = readGame contents
    return gs
-}
putWinner :: GameState -> IO ()
putWinner gs = do
    let winner = whoWillWin gs
    putStrLn (show winner)
--
--
--
{-
Edge cases:
length holes <  6 -done?
nums < 0 -probably done for holes, needed for stores
excess lines?
if lines are out of order
fewer than required lines
extra empty lines
}