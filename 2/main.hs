import System.IO
import Data.List

main = do  
    contents <- readFile "input1"
    let items = lines contents
    
    -- print $ [words x | x <- items]
    print $ sum [getScore (words x) | x <- items]
    print $ sum [getScore2 (words x) | x <- items]
 
getScore x = battleScore x + handScore (x !! 1)

handScore :: String -> Int
handScore "X" = 1
handScore "Y" = 2
handScore "Z" = 3
handScore x   = 0

battleScore :: [String] -> Int
battleScore x
    | (head x == "A") && (x !! 1 == "Y") = 6
    | (head x == "B") && (x !! 1 == "Z") = 6
    | (head x == "C") && (x !! 1 == "X") = 6
    | (head x == "A") && (x !! 1 == "X") = 3
    | (head x == "B") && (x !! 1 == "Y") = 3
    | (head x == "C") && (x !! 1 == "Z") = 3
    | otherwise = 0

{-
A,X Rock
B,Y Paper
C,Z Scissors
-}

-- part 2

{-
A Rock
B Paper
C Scissors
X lose
Y draw
Z win
-}

getScore2 x = battleScore2 (x !! 1) + handScore2 x

battleScore2 :: String -> Int
battleScore2 "X" = 0
battleScore2 "Y" = 3
battleScore2 "Z" = 6
battleScore2 x   = 0

handScore2 :: [String] -> Int
handScore2 x
    | (head x == "A") && (x !! 1 == "X") = 3
    | (head x == "A") && (x !! 1 == "Y") = 1
    | (head x == "A") && (x !! 1 == "Z") = 2
    | (head x == "B") && (x !! 1 == "X") = 1
    | (head x == "B") && (x !! 1 == "Y") = 2
    | (head x == "B") && (x !! 1 == "Z") = 3
    | (head x == "C") && (x !! 1 == "X") = 2
    | (head x == "C") && (x !! 1 == "Y") = 3
    | (head x == "C") && (x !! 1 == "Z") = 1
    | otherwise = 0