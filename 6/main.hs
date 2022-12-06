{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use infix" #-}
import System.IO
import Data.List
import Data.List.Split (splitOn)

main = do  
    contents <- readFile "input1"
    let items = lines contents

    let all4Char = [ [ take 4 (snd (splitAt i xs)) | i <- [0..length xs - 5]] | xs <- items]
    let isMarkerList = [ map compare4chars xs | xs <- all4Char]
    print $ [ 4 + resolveMaybe (elemIndex True xs) | xs <- isMarkerList ]
    print $ [ 14 + resolveMaybe (elemIndex True xs) 
        | xs <- [ map (compareNchars 14) xs 
            | xs <- [ [ take 14 (drop i xs)
                | i <- [0..length xs - 15]] 
                    | xs <- items]] ]


readBuf longstr
    | length longstr < 4 = False
    | otherwise = readBuf (tail longstr)


compare4chars str
    | length str < 4 = False
    | length (nub str) < 4 = False
    | length (nub str) == 4 = True

compareNchars :: Eq a => Int -> [a] -> Bool
compareNchars n str
    | length str < n = False
    | length (nub str) < n = False
    | length (nub str) == n = True

resolveMaybe :: Num a => Maybe a -> a
resolveMaybe Nothing = 0
resolveMaybe (Just x) = x