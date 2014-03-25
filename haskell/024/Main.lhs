
A permutation is an ordered arrangement of objects. For example, 3124
is one possible permutation of the digits 1, 2, 3 and 4. If all of the
permutations are listed numerically or alphabetically, we call it
lexicographic order. The lexicographic permutations of 0, 1 and 2 are:

012 021 102 120 201 210

What is the millionth lexicographic permutation of the digits 0, 1, 2,
3, 4, 5, 6, 7, 8 and 9?

> module Main where

> import Prelude 
> import qualified IO
> import qualified Data.List as List
> import qualified System

> lexp :: Int -> [Int]
> lexp n = List.sort (List.permutations [0 .. 9]) !! n

> flatten :: [Int] -> Integer
> flatten l = read (concat (map show l))

> main :: IO ()
> main = 
>   do args <- System.getArgs 
>      case args of
>       [] -> print "Usage: Euler <int>" 
>       n : _ -> putStr ("The " ++ show n ++ "th lexicographic permutation is " ++ (show $ flatten $ lexp $ read n - 1))

$ time Euler 1000000
The "1000000"th lexicographic permutation is 2783915460
real	0m14.435s
user	0m12.860s
sys	0m1.366s
