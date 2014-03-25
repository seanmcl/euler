
There are 1111 ways in which five 6-sided dice (sides numbered 1 to 6)
can be rolled so that the top three sum to 15. Some examples are:

D_(1),D_(2),D_(3),D_(4),D_(5) = 4,3,6,3,5
D_(1),D_(2),D_(3),D_(4),D_(5) = 4,3,3,5,6
D_(1),D_(2),D_(3),D_(4),D_(5) = 3,3,3,6,6
D_(1),D_(2),D_(3),D_(4),D_(5) = 6,6,3,3,3

In how many ways can twenty 12-sided dice (sides numbered 1 to 12) be
rolled so that the top ten sum to 70?

> module Main where

> import Prelude 
> import qualified List
> import qualified System
> import qualified IO
> import qualified EulerLib as Lib

First try a simple enumeration approach to make sure 
I understand the problem.  Should have 

combs 5 6 3 15 = 1111

Challenge: find 

combs 20 12 10 70

largest k l returns the largest k elements of l

> largest :: Int -> [Int] -> [Int]
> largest k l = largestAux (take k sl) (drop k sl)
>   where sl = List.sort l
>         largestAux acc [] = acc
>         largestAux acc (h:t) = 
>           case Lib.findRem (< h) acc of
>             Nothing -> largestAux acc t
>             Just(_, acc') -> largestAux (Lib.insert h acc') t

> rolls :: Int -> Int -> [[Int]]
> rolls numDice numSides = 
>   Lib.allCombs $ replicate numDice [1 .. numSides]

> combs :: Int -> Int -> Int -> Int -> Int
> combs numDice numSides numTop topSum = 
>   let rs0 = rolls numDice numSides
>       rs1 = map (sum . largest numTop) rs0
>       rs2 = filter (== topSum) rs1 in
>   length rs2

OK, we get the right answer now:

$ time Euler 5 6 3 15
1111
real	0m0.016s
user	0m0.012s
sys	0m0.003s

But enumeration has no chance on this problem:

12^20 = 3833759992447475122176

$ time Euler 6 12 10 70
21
real	0m4.025s
user	0m3.922s
sys	0m0.042s

$ time Euler 7 12 10 70
38564
real	0m56.174s
user	0m55.206s
sys	0m0.522s

> main :: IO ()
> main = 
>   do args <- System.getArgs 
>      case args of
>        numDice : numSides : numTop : topSum : _ -> 
>          putStr $ show $ combs (read numDice) (read numSides) (read numTop) (read topSum)
>        _ -> print "Usage: Euler <numDice:int> <numSides:int> <numTop:int> <topSum:int>" 
