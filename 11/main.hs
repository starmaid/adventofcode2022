{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use infix" #-}
import System.IO
import Data.List
import Data.List.Split (splitOn, dropInitBlank)
import Data.Maybe (maybe, fromJust, fromMaybe)
import Data.Function (on)

import Debug.Trace (trace)
debug :: c -> String -> c
debug = flip trace

main = do  
    contents <- readFile "input"
    let itemsa = lines contents

    let monks = map parseMonk (splitOn [""] itemsa)
    --let oneround = (foldl monkeyCyc monks [0..3])
    -- let oneround = monkeyCyc monks (monks !! 0)
    -- let tworound = monkeyCyc oneround (oneround !! 1)
    -- let thrround = monkeyCyc tworound (tworound !! 2)
    -- let forround = monkeyCyc thrround (thrround !! 3)
    -- let r = recRun monks 20
    let r = recRun monks 10000
    --print r
    let screens = unlines (map (unlines . (map show)) (monks:r))
    -- putStr screens
    putStr (unlines (map show (last r)))
    
{-
115025}
93029}
114545}
114164}
8590}
30465}
122693}
122667}
-}


recRun :: [Monk] -> Int -> [[Monk]]
recRun round i
    | i == 1 = [doneround]
    | otherwise = doneround:(recRun doneround (i-1))
    where doneround = (foldl monkeyCyc round [0..((length round)-1)])


monkeyCyc :: [Monk] -> Int -> [Monk] 
monkeyCyc allmonk i = foldl (evalItem thismonk) allmonk (items thismonk)
    where thismonk = allmonk !! i
    -- go through each item
    
evalItem :: Monk -> [Monk] -> Int -> [Monk]
evalItem thismonk allmonk item = allmonk'
    -- calculate new worry value
    -- divide by three and round down to int
    -- evaluate based on test
    -- thismonk is not most recent: (allmonk !! (num thismonk)
    where tmonk = allmonk !! (num thismonk)
          newv = doOpCond item (head (op tmonk)) ((op tmonk) !! 1)
          testr = 0 == mod newv (test tmonk)
          (i, m, v) = if testr
                    then (itrue tmonk, allmonk !! (itrue tmonk), (newv)) 
                    else (ifalse tmonk, allmonk !! (ifalse tmonk), newv)
          (j, newself, fac) = (num tmonk, 
                        Monk (num tmonk) ([]) 
                        (op tmonk) (test tmonk)
                        (itrue tmonk) (ifalse tmonk) (counter tmonk + 1)
                        , test tmonk)
          reciever = Monk (num m) (items m ++ [v]) (op m) (test m)
                    (itrue m) (ifalse m) (counter m)
          allmonk' = insert newself (dropItemNum j
            (insert reciever (dropItemNum i allmonk)))
            
dropItemNum :: Int -> [a] -> [a]
dropItemNum i l = take i l ++ drop (i+1) l
 
doOpCond :: Int -> String -> String -> Int
doOpCond item op val
    | op == "+" = (item + (read val :: Int)) `mod` (5*17*2*7*3*11*13*19)
    | op == "*" = if val == "old"
        then (item * item) `mod` (5*17*2*7*3*11*13*19)
        else (item * (read val :: Int)) `mod` (5*17*2*7*3*11*13*19)
 
doOpCond2 :: Int -> String -> String -> Int
doOpCond2 item op val
    | op == "+" = (item + (read val :: Int)) `div` 3
    | op == "*" = if val == "old"
        then (item * item) `div` 3
        else (item * (read val :: Int)) `div` 3

parseMonk :: [[Char]] -> Monk
parseMonk a = Monk (read (init (drop 7 (head a))) :: Int)
                    (map (\a -> read a :: Int) (splitOn ", " (drop 18 (a !! 1))))
                    (splitOn " " (drop 23 (a !! 2)))
                    (read (drop 21 (a !! 3)) :: Int)
                    (read (drop 29 (a !! 4)) :: Int)
                    (read (drop 30 (a !! 5)) :: Int)
                    0

data Monk = Monk { num :: Int
                    , items :: [Int]
                    , op :: [String]
                    , test :: Int
                    , itrue :: Int
                    , ifalse :: Int
                    , counter :: Int
                    } deriving (Show, Eq, Ord)