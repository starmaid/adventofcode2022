{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use infix" #-}
import System.IO
import Data.List
import Data.List.Split (splitOn)
import Data.Maybe (maybe, fromJust, fromMaybe)
import Data.Function (on)

import Debug.Trace (trace)
debug :: c -> String -> c
debug = flip trace

main = do  
    contents <- readFile "input1"
    let items = lines contents

    -- find start
    let sCols = [fromMaybe (-1) (elemIndex 'S' row) | row <- items]
    let sRow = fromMaybe (-1) (findIndex (>= 0) sCols)
    let sCol = sCols !! sRow
    print $ (sCol, sRow)

    -- start traversing
    let path = doDfs items [(-1,-1)] (sCol,sRow)
    print path 
    print $ (length path) -1

-- find start

-- this needs a DFS graph traversal
-- OOPS YOU NEED TO BFS

-- for each node

-- check if destination is adjacent
-- there are possible destinations
-- and then after only the possibles, have to pick the best

-- TOP LEFT IS 0,0
-- ROW = Y
-- COL = X

doDfs :: [[Char]] -> [(Int, Int)] -> (Int, Int) -> [(Int, Int)]
doDfs graph path (x,y) 
    -- | length path > 60 = path
    | ce == 'z' && 'E' == fst (fromMaybe ('~',(-1,-1)) (find (\n -> fst n == 'E') nodes)) = path++[(-1,-1)] `debug` ("found E "++(show (length path)))
    | null fnodes = path
    | otherwise = if null shortestpath then path else shortestpath
    where c = getCoord graph (x,y)
          ce = if c == 'S' then '`' else c
          nodes = sort (getAdjList graph path (x,y))
          fnodes = reverse (filter (\n -> (succ ce) >= (fst n)) nodes)
          results = [doDfs graph (path++[(x,y)]) (snd n) | n <- fnodes]
          resultsf = filter (\l -> (last l) == (-1,-1)) results
          shortestpath = if null resultsf then [] else minimumBy (compare `on` length) resultsf


getAdjList :: [[Char]] -> [(Int, Int)] -> (Int, Int) -> [(Char, (Int, Int))]
getAdjList graph path (x,y) = [(getCoord graph p, p) | p <- points]
    where adj = [(1,0), (-1,0), (0,1), (0,-1)]
          points = [(a+x, b+y) | (a,b) <- adj] \\ path

getCoord :: [[Char]] -> (Int, Int) -> Char
getCoord graph (x,y)
    | y < 0 || y >= (length graph) = '~'
    | x < 0 || x >= (length (head graph)) = '~'
    | otherwise = (graph !! y) !! x