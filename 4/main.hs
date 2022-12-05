{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
import System.IO
import Data.List

main = do  
    contents <- readFile "input1"
    let items = lines contents

    print $ [ intersectLofL [ rangeFromList (splitOnChar ys '-') | ys <- splitOnChar xs ','] | xs <- items]


splitOnChar :: Eq a => [a] -> a -> [[a]]
splitOnChar xs c = fixTuple (splitAt (resolveMaybe (elemIndex c xs)) xs)

resolveMaybe :: Num a => Maybe a -> a
resolveMaybe Nothing = 0
resolveMaybe (Just x) = x

fixTuple :: ([a], [a]) -> [[a]]
fixTuple (a,b) = [a, tail b]

rangeFromList :: [String] -> [Int]
rangeFromList [x,y] = [read x .. read y]

intersectLofL :: Eq a => [[a]] -> [a]
intersectLofL [x,y] = intersect x y