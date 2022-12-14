{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use infix" #-}
import System.IO
import Data.List
import Data.List.Split (splitOn)

main = do  
    contents <- readFile "input1"
    let items = lines contents

    print items
    let question = splitArndThing "" items
    print question
    let cargo = map (dropWhile (==' ')) (transpose [ map (!! 1) xs | xs <- map (splitBy3Col) (init (head question))])
    print cargo
    let instString = map (odds . splitArndThingR ' ') 
                       (head (tail question))
    let insts = [ [read y :: Int | y <- x] | x <- instString]
    print $ foldl applyMove cargo insts
    print $ map (head) (foldl applyMove cargo insts)

applyMove stk mv = [ newcol is | is <- [0..length stk - 1]]
    where   (take,leave) = splitAt numcrates (stk !! fromL)
            numcrates = head mv
            fromL = mv !! 1 - 1
            toL = last mv - 1
            newcol i
                | i == fromL = leave
                -- | i == toL = (reverse take) ++ (stk !! toL)
                | i == toL = take ++ (stk !! toL)
                | otherwise = stk !! i

evens (x:xs) = x:odds xs
evens _ = []

odds (_:xs) = evens xs
odds _ = []

splitBy3Col :: [a] -> [[a]]
splitBy3Col xs
    | length xs == 3 = [xs]
    | otherwise = head3 : splitBy3Col tail3
    where ys = splitAt 3 xs
          head3 = fst ys
          tail3 = snd ys
    

splitArndThingR :: Eq a => a -> [a] -> [[a]]
splitArndThingR c xs
    | notElem c xs = [xs]
    | otherwise = head' : splitArndThingR c tail'
    where ys = splitAt i xs
          i = resolveMaybe (elemIndex c xs)
          head' = fst ys
          tail' = snd ys

splitArndThing :: Eq a => a -> [a] -> [[a]]
splitArndThing c xs = fixTuple (splitAt (resolveMaybe (elemIndex c xs)) xs)

resolveMaybe :: Num a => Maybe a -> a
resolveMaybe Nothing = 0
resolveMaybe (Just x) = x

fixTuple :: ([a], [a]) -> [[a]]
fixTuple (a,b) = [a, tail b]
