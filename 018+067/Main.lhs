
By starting at the top of the triangle below and moving to adjacent
numbers on the row below, the maximum total from top to bottom is 23.

3
7 5
2 4 6
8 5 9 3

That is, 3 + 7 + 4 + 9 = 23.

Find the maximum total from top to bottom of the triangle below:

75
95 64
17 47 82
18 35 87 10
20 04 82 47 65
19 01 23 75 03 34
88 02 77 73 07 63 67
99 65 04 28 06 16 70 92
41 41 26 56 83 40 80 70 33
41 48 72 33 47 32 37 16 94 29
53 71 44 65 25 43 91 52 97 51 14
70 11 33 28 77 73 17 78 39 68 17 57
91 71 52 38 17 14 91 43 58 50 27 29 48
63 66 04 68 89 53 67 30 73 16 69 87 40 31
04 62 98 27 23 09 70 98 73 93 38 53 60 04 23

NOTE: As there are only 16384 routes, it is possible to solve this
problem by trying every route. However, Problem 67, is the same
challenge with a triangle containing one-hundred rows; it cannot be
solved by brute force, and requires a clever method! ;o)

> module Main where

> import Prelude 
> import qualified System
> import qualified IO

> zipT :: [Int] -> [Int] -> [Int]
> zipT [] [] = []
> zipT [e1] [e2] = [e1 + e2]
> zipT (e11 : e12 : t1) (e21 : e22 : t2) = 
>      (max e11 e12 + e21) : zipT (e12 : t1) (e22 : t2)
> zipT _ _ = error "Impossible" 

> zipAll :: [[Int]] -> [Int]
> zipAll [l] = l
> zipAll (l1 : l2 : ls) = zipAll ((zipT (0 : l1) l2) : ls)
> zipAll _ = error "Impossible" 

> maxPath :: [[Int]] -> Int
> maxPath = maximum . zipAll

> parse :: String -> IO [[Int]]
> parse f = do
>   s <- IO.readFile f
>   let ws = map (map read . words) (lines s)
>   return ws

> main :: IO ()
> main = 
>   do args <- System.getArgs 
>      case args of
>       [] -> print "Usage: Euler filename" 
>       h : _ -> 
>         do ws <- parse h
>            print ("The longest path is: " ++ show (maxPath ws))

~/save/versioned/projects/compiler/euler/18+67
$ time Euler 18.txt
"The longest path is: 1074"

real	0m0.006s
user	0m0.002s
sys	0m0.003s

~/save/versioned/projects/compiler/euler/18+67
$ time Euler 67.txt
"The longest path is: 7273"

real	0m0.051s
user	0m0.034s
sys	0m0.006s
