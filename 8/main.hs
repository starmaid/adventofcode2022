{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use infix" #-}
import System.IO
import Data.List
import Data.List.Split (splitOn)
import Data.Maybe (maybe, fromJust, fromMaybe)
import Data.Function (on)

main = do  
    contents <- readFile "input"
    let items = lines contents
    let rows = [[read [i] | i <- s] | s <-items]
    let cols = transpose rows
    --print rows
    --print cols
    print $ [[isVisible rows cols i j | i <- [0..length cols-1]] 
                    | j <- [0..length rows-1]]
    print $ length (filter (True ==) 
        [isVisible rows cols i j 
            | i <- [0..length cols-1], j <- [0..length rows-1]])
    -- 1870 is not the right answer. right answer for someone else
    print $ maximum 
        [countVisible rows cols i j 
            | i <- [0..length cols-1], j <- [0..length rows-1]]
    
countVisible :: [[Int]] -> [[Int]] -> Int -> Int -> Int
countVisible rows cols x y = 
    product [
        -- from left
        length (takeWhile (< height) llist) + islEdge,
        -- from right
        length (takeWhile (< height) rlist) + isrEdge,
        -- from up
        length (takeWhile (< height) ulist) + isuEdge,
        -- from down
        length (takeWhile ( < height) dlist) + isdEdge
    ]
    where height = rows !! y !! x
          llist = reverse (take x (rows !! y))
          rlist = drop (x+1) (rows !! y)
          ulist = reverse (take y (cols !! x))
          dlist = drop (y+1) (cols !! x)
          islEdge = if maximum' llist >= height then 1 else 0
          isrEdge = if maximum' rlist >= height then 1 else 0
          isuEdge = if maximum' ulist >= height then 1 else 0
          isdEdge = if maximum' dlist >= height then 1 else 0

isVisible :: [[Int]] -> [[Int]] -> Int -> Int -> Bool
isVisible rows cols x y = 
    or [
        -- from left
        maximum' (take x (rows !! y)) < height,
        -- from right
        maximum' (drop (x+1) (rows !! y)) < height,
        -- from top
        maximum' (take y (cols !! x)) < height,
        -- from bottom
        maximum' (drop (y+1) (cols !! x)) < height
    ]
    where height = rows !! y !! x

maximum' :: [Int] -> Int
maximum' list
    | null list = -1
    | otherwise = maximum list