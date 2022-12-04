import System.IO

main = do  
    contents <- readFile "input1.txt"
    let items = lines contents
    
    print items

