{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use infix" #-}
import System.IO
import Data.List

main = do  
    contents <- readFile "input1"
    let items = lines contents

    --print $ [ [ rangeFromList (splitOnChar ys '-') | ys <- splitOnChar xs ','] | xs <- items]
    let listOfRanges = [ [ rangeFromList (splitOnChar ys '-') | ys <- splitOnChar xs ','] | xs <- items]
    
    print $ length (filter (fullContains) listOfRanges)
    print $ length (filter (anyContains) listOfRanges)



splitOnChar :: Eq a => [a] -> a -> [[a]]
splitOnChar xs c = fixTuple (splitAt (resolveMaybe (elemIndex c xs)) xs)

resolveMaybe :: Num a => Maybe a -> a
resolveMaybe Nothing = 0
resolveMaybe (Just x) = x

fixTuple :: ([a], [a]) -> [[a]]
fixTuple (a,b) = [a, tail b]

rangeFromList :: [String] -> [Int]
rangeFromList [x,y] = [read x .. read y]

fullContains :: Eq a => [[a]] -> Bool
fullContains [x,y]
    | intersect x y == x = True
    | intersect x y == y = True
    | otherwise = False

anyContains :: Eq a => [[a]] -> Bool
anyContains [x,y]
    | null (intersect x y) = False
    | otherwise = True


{-
-- realized filter would be better than counting nulls

intersectLofL :: Eq a => [[a]] -> [a]
intersectLofL [x,y] = intersect x y

isNull :: (Foldable t, Num a1) => t a2 -> a1
isNull x
    | null x = 0
    | otherwise = 1
-}