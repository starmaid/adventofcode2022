{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use infix" #-}
import System.IO
import Data.List
import Data.List.Split (splitOn, chunksOf)
import Data.Maybe (maybe, fromJust, fromMaybe)
import Data.Function (on)

main = do  
    contents <- readFile "input"
    let items = lines contents

    let ops =  [ if length x == 2 then (head x, read (x !! 1) ::Int) else (head x, 0) | x <- map (splitOn " ") items]
    let values = [(1,0,0)]
    let reghist = foldl doOp values ops
    --print reghist
    let part1 = sum [(\(a,b,c) -> a*b) (reghist !! x) | x <- [20,60..220]]
    -- print $ (chunksOf 40 (tail reghist))
    let screens = unlines (map makeRow (chunksOf 40 (tail reghist)))
    putStr screens

-- a is the register, b is the cycle number, 
-- c is the number to add to the register on the next cycle
doOp :: (Num a) => [(a, a, a)] -> (String, a) -> [(a, a, a)]
doOp values (op, n) 
    | op == "addx" = values ++ [(reg+inp, cyc+1, 0), (reg+inp, cyc+2, n)]
    | op == "noop" = values ++ [(reg+inp, cyc+1, 0)]
    where (reg, cyc, inp) = last values

makeRow :: Integral a => [(a, a, c)] -> [Char]
makeRow l = concat [if abs (a-((b-1) `mod` 40)) < 2 then "#" else " " | (a,b,_) <- l]