import System.IO
import Data.List

main = do  
    contents <- readFile "input"
    let items = lines contents

    print $ sum [getPriority (getTupleInter (splitAt (length xs `div` 2) xs)) | xs <- items]

    print $ sum [getPriority x | x <- map intersect3 (groupElf items)]


groupElf :: [a] -> [[a]]
groupElf xs
    | length xs == 3 = [xs]
    | otherwise = head3:groupElf tail3
    where (head3, tail3) = splitAt 3 xs

intersect3 :: Eq a => [[a]] -> [a]
intersect3 xs = intersect (head xs) (intersect (xs !! 1) (xs !! 2))

{-
["s4f", "s3f", "s1f", "adf", "sff", "sdC", "sss", "s5f", "1df"]

(["s4f","s3f","s1f"],["adf","sff","sdC","sss","s5f","1df"])

-}

getTupleInter :: Eq a => ([a], [a]) -> [a]
getTupleInter = uncurry intersect

getPriority ::  String -> Int
getPriority c = head [ snd x | x <- zip ['a'..'z'] [1..26] ++ zip ['A'..'Z'] [27..52] , fst x == head c ]
