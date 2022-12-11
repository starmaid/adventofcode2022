# cheat sheet

## ghci

```haskell
:l filename -- load file
:r -- reload all files
:m + Data.List -- import into global namespace

:break 2 -- add breakpoint at line 2
:continue
:step
:delete *
:delete 23

```

### booleans

```haskell
&& -- and
|| -- or
not -- not
```

conditionals
```
==
\=
>
<
>=
<=
```

## basic

```haskell
succ x -- takes next of a Num type
max [list]
min [list] 
div x y -- integer division
odd x
even x

let name = whatever --

-- to and from types
read "4"
4
show 4
"4"

-- lowest precedence function apply
func (x ++ "bunch of operations to x")
func $ x ++ "bunch of operations to x"

-- infix notation
func x y 
x `func` y

-- function composition: all are valid
funcA (funcB x)
( funcA . funcB ) x
funcA . funcB $ x
```

## functions

```haskell
-- plain
funcname param1 param2 = operation

-- pattern matching: 
-- determines if a value conforms to some form
factorial :: (Integral a) => a -> a  
factorial 0 = 1  
factorial 1 = 1
factorial n = n * factorial (n - 1)  

-- guards:
-- test whether some property is true
funcname param1 param2
    | condition = operation
    | condition = operation
    | otherwise = operation
    where a = param1 + param2
          b = param1 * 3
```

## if

```haskell
doubleSmallNumber x = if x > 100  
                        then x  
                        else x*2
doubleSmallNumber' x = (if x > 100 then x else x*2) + 1
```


## lists

```haskell
let something = [1,2,3]
[1..20] -- list from 1 to 20
[2,4..20] -- 2,4,6, etc. only arithmetic, only first two vals.
--works on letters
[1,2,3] ++ [4,5,6] = [1,2,3,4,5,6]
1:[2,3] = [1,2,3]
[1,2,3] !! 0 = 1 -- get element, zero inexed

head [list] -- gets first element
tail [list] -- gets everything but first
last [list] -- gets last
init [list] -- gets everything but last
length [list] -- returns length
null [list] -- returns true if list is empty
reverse -- returns reversed list
take x [list] -- returns first x elements of list
drop x [list] -- returns elements after the first x
maximum [list] -- max
minimum [list] -- min
sum [list] -- sum of elements
product [list]
elem x [list] -- returns true if x in list
cycle [list] -- cycles list infinitely
repeat x -- repeats one value infinitely
replicate x y -- repeats y x-times
```

### comprehensions

```haskell
[x*2 | x <- [1..10]]
[2,4,6,8,10,12,14,16,18,20]

[    x*2    | x <- [1..10] , x*2 >= 12 ]
[ operation |    source    , predicate ]
[12,14,16,18,20]

-- draw from multiple lists
[ x*y | x <- [2,5,10], y <- [8,10,11]]
```


### Data.List

http://learnyouahaskell.com/modules#data-list

```haskell
import Data.List

-- variants that return Num instead of Int so you can do better math without converting
genericLength
genericTake
genericDrop
genericSplitAt
genericIndex
genericReplicate

inits
tails
isInfixOf
isPrefixOf
isSuffixOf

elem
notElem

find (predicate) [list] -- returns a maybe with the first element that satisfies that predicate
delete x [list]-- deletes first occurance of x
elemIndex
elemIndices
findIndex
findIndexes

intersperse
intercalate
transpose
concat
concatmap

-- sets
nub -- removes duplicates
[list1] \\ [list2] -- difference: removes elements in list2 from list1, if present
union [list1] [list2]
intersect [list1] [list2]

-- you can specify the operation that decides how things are equal.
-- nub is the same as nubBy (==)
nubBy (predicate) [list1] [list2]
deleteBy (predicate) [list1] [list2]
unionBy (predicate) [list1] [list2]
intersectBy (predicate) [list1] [list2]
groupBy (predicate) [list1] [list2]


-- define your own ordering function
sortBy
insertBy
maximumBy
minimumBy



-- logic
and [list] -- returns True if all elements are True
or -- returns True if any value is True
all (predicate) [list] 
any (==4) [list] 

-- 
iterate (function) startingValue -- creates an infinite list by repeatedly applying the function
take 10 $ iterate (*2) 1  
[1,2,4,8,16,32,64,128,256,512]


splitAt x [list] -- splits list into tuple of lists at element x

takeWhile (predicate) [list] -- returns a list of the first elements of the list that matched the predicate, and then stops.
ghci> takeWhile (/=' ') "This is a sentence"  
"This"

span (predicate) [list] -- takeWhile, but returns a tuple of first part and last part

break (predicate) [list] -- does the same thing as span, but splits the list when predicate is first true instead of false.

dropWhile (predicate) [list] -- ignores values while predicate is true, and then when first value is false, returns rest of list
ghci> dropWhile (/=' ') "This is a sentence"  
" is a sentence" 

sort [list] -- you get it.
insert x [list] -- inserts x in list at the last location it is less than or equal to the next element

group [list] -- groups adjacent elements if they are equal
ghci> group [1,1,1,1,2,2,2,2,3,3,2,2,2,5,6,7]
[[1,1,1,1],[2,2,2,2],[3,3],[2,2,2],[5],[6],[7]]

partition
ghci> partition (`elem` ['A'..'Z']) "BOBsidneyMORGANeddy"  
("BOBMORGAN","sidneyeddy")

-- files stuff
lines
unlines
words
unwords

```

## Functions

```haskell
map (f), [list] -- applies function to all elements of list

filter (p) [list] -- returns a list of elements that satisfy the predicate

-- run a function (two inputs), and accumulate state that is passed to the function
foldl (f acc x) (starting value) [list] -- left -> right
foldr (f acc x) (starting value) [list] -- right -> left

sum' xs = foldl (\acc x -> acc + x) 0 [3,5,2,1]
11

-- same as fold, but assumes initial value is the first element of the array
foldl1 (f acc x) [list]
foldr1 (f acc x) [list]

-- just like fold, but creates an array of the accumulator instead of just consuming it
scanl (f acc x) (starting value) [list]
scanr (f acc x) (starting value) [list]

scanl (+) 0 [3,5,2,1] 
[0,3,8,10,11]

scanl1 (f acc x) [list]
scanr1 (f acc x) [list]
```