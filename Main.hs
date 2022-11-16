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

getPlayer :: String -> Player
getPlayer str =
    case str of 
        "1" -> Player1
        "2" -> Player2
        -- WILL NEED TO ADD LATER : _ -> Nothing 

getHoles :: String -> [Int] -> [Hole]
getHoles str labels = 
    let stringHoles = splitOn " " str
        numBeans = map (\stringBeans -> read stringBeans :: Int) stringHoles
    in  zip labels numBeans
        
getStore :: String -> Store
getStore str = (read str :: Store)

readGame :: String -> GameState
readGame inputGS =  
    let playerLine:s1Line:h1Line:s2Line:h2Line:[] = lines inputGS
    in  (getPlayer playerLine, Board (getStore s1Line) (getHoles h1Line [1..6]) (getStore s2Line) (getHoles h2Line [7..12]))
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

loadGame :: FilePath -> IO GameState
loadGame file = do
    contents <- readFile file
    let gs = readGame contents
    return gs

    -- do
    --     contents <- readFile file
    --     let data = readGame contents
    --     putStrLn data

putWinner :: GameState -> IO ()
putWinner gs = do
    let winner = whoWillWin gs
    putStrLn (show winner)
--
--
--
