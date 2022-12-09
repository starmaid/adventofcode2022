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
    contents <- readFile "input"
    let items = lines contents

    let moves = [(head s, read $ head $ tail s :: Int) | s <- map (splitOn " ") items]
    let moves1 = concat [ replicate (snd m) (fst m, 1) | m <- moves]

    let h = (0,0)
    let t = (0,0)
    {-
    let positions = scanl (moveR) [h,t] moves1

    print $ length moves1
    print $ length positions

    let upos = nub (map (!! 1) positions)

    print upos
    print $ length upos
    -}
    -- part 2

    let positions = scanl (moveRp) (replicate 10 h) moves1

    
    print $ length moves1
    --print positions

    let upos = nub (map (!! 9) positions)

    --print upos
    print $ length upos


moveRp :: [(Int, Int)] -> (String, Int) -> [(Int, Int)]
moveRp points (dir,dis)
    | dir == "R" = foldl (\acc m -> acc++[newT (last acc) m]) [(hx+dis,hy)] (tail points)
    | dir == "L" = foldl (\acc m -> acc++[newT (last acc) m]) [(hx-dis,hy)] (tail points)
    | dir == "U" = foldl (\acc m -> acc++[newT (last acc) m]) [(hx,hy+dis)] (tail points)
    | dir == "D" = foldl (\acc m -> acc++[newT (last acc) m]) [(hx,hy-dis)] (tail points)
    where (hx,hy) = head points


moveR :: [(Int, Int)] -> (String, Int) -> [(Int, Int)]
moveR [(hx,hy),(tx,ty)] (dir,dis)
    | dir == "R" = [(hx+dis,hy), newT (hx+dis,hy) (tx,ty)]
    | dir == "L" = [(hx-dis,hy), newT (hx-dis,hy) (tx,ty)]
    | dir == "U" = [(hx,hy+dis), newT (hx,hy+dis) (tx,ty)]
    | dir == "D" = [(hx,hy-dis), newT (hx,hy-dis) (tx,ty)]


newT :: (Int, Int) -> (Int, Int) -> (Int, Int)
newT (hx,hy) (tx,ty)
    -- dont move
    | abs hdist < 2 && abs vdist < 2 = (tx,ty)
    -- move in only one axis
    | abs hdist == 2 && abs vdist < 2 = (tx+hdist+hadj,ty+vdist)
    | abs hdist < 2 && abs vdist == 2 = (tx+hdist,ty+vdist+vadj)
    -- make a diagonal move
    | abs hdist == 2 && abs vdist == 2 = (tx+hdist+hadj,ty+vdist+vadj)
    | otherwise = (tx+hdist,ty+vdist) 
    where hdist = hx - tx --`debug` (""++(show hx)++" "++(show hy)++" "++(show tx)++" "++(show ty))
          vdist = hy - ty
          hadj = if hdist < 0
                then 1
                else (-1)
          vadj = if vdist < 0
                then 1
                else (-1)