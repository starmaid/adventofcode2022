{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use infix" #-}
import System.IO
import Data.List
import Data.List.Split (splitOn)
import Data.Maybe (maybe, fromJust, fromMaybe)

main = do  
    contents <- readFile "input1"
    let items = lines contents

    --print items
    let root = Tnode "/" 0 "" [] []
    let (listl, head) = foldl (parseLine) ([root], root) items
    print listl
    print $ sum (filter (<=100000) (map (getweight listl) listl))
    -- 746558 is too low.

getweight :: [Tnode] -> Tnode -> Int
getweight list node
    | null (children node) = sum (map (fst) (files node))
    | otherwise = sum (map (fst) (files node)) + sum [getweight list (getNodeByName x list) | x <- (children node)]

parseLine :: ([Tnode],Tnode) -> String -> ([Tnode],Tnode)
parseLine (ftree,cnode) linestr
    -- set current node to parent of current node
    | take 7 linestr == "$ cd .." 
        = (ftree
            , getNodeByName (parent cnode) ftree)
    -- if cd, change what the active node is
    | take 5 linestr == "$ cd " 
        = (ftree
            , getNodeByName (drop 5 linestr) ftree)
    -- if ls, dont do anything. prepare to add things to node
    | take 5 linestr == "$ ls " = (ftree,cnode)
    -- jf dir, create a directory node
    -- add it to the ftree
    -- and set a new child on the parent
    | take 4 linestr == "dir " 
        = ( (Tnode nodename 0 (name cnode) [] [])
                :addChild nodename cnode
                :removeNode cnode ftree
            , Tnode (name cnode) 0 (parent cnode) (nodename:(children cnode)) (files cnode))
    -- otherwise, its a file
    | head linestr `elem` ['0'..'9']= ((addFile cnode linestr)
                    :(removeNode cnode ftree)
                    ,addFile cnode linestr)
    | otherwise = (ftree,cnode)
    where root = Tnode "" 0 "" [] []
          nodename = drop 4 linestr


addFile :: Tnode -> String -> Tnode
addFile cnode str = Tnode (name cnode) (weight cnode) (parent cnode) (children cnode) ((parseFile str):(files cnode))

parseFile :: String -> (Int, String)
parseFile str = (read (head sarr) :: Int, last sarr)
    where sarr = splitOn " " str

addChild :: String -> Tnode -> Tnode
addChild nodename cnode = Tnode (name cnode) (weight cnode) (parent cnode) (nodename:(children cnode)) (files cnode)

removeNode :: Tnode -> [Tnode] -> [Tnode]
removeNode cnode ftree = [ x | x <- ftree, not (nodeMatchName (name cnode) x)]

getNodeByName :: Foldable t => String -> t Tnode -> Tnode
getNodeByName nname ftree = fromMaybe root (find (nodeMatchName nname) ftree)
    where root = Tnode "" 0 "" [] []

nodeMatchName :: String -> Tnode -> Bool
nodeMatchName s n = name n == s

data Tnode = Tnode  { name :: String
                    , weight :: Int
                    , parent :: String
                    , children :: [String]
                    , files :: [(Int, String)]
                    } deriving (Show)