
It can be seen that the number, 125874, and its double, 251748,
contain exactly the same digits, but in a different order.

Find the smallest positive integer, x, such that 2x, 3x, 4x, 5x, and
6x, contain the same digits.

> module Main where

> import Prelude 
> import qualified Data.List as List

> curious :: Integer -> Bool
> curious n = 
>   let ns = map (List.sort . show . (n *)) [2 .. 5] in
>   length (List.nub ns) == 1

> solution :: Maybe Integer
> solution = List.find curious [1 ..]

> main :: IO ()
> main = putStr ("Curious: " ++ show solution)

$ time ./Euler
Curious: Just 142857
real	0m0.665s
user	0m0.650s
sys	0m0.008s

