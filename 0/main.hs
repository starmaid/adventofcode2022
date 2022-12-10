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
    contents <- readFile "input1"
    let items = lines contents

    print items