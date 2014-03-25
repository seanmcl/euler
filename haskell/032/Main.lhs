
We shall say that an n-digit number is pandigital if it makes use of
all the digits 1 to n exactly once; for example, the 5-digit number,
15234, is 1 through 5 pandigital.

The product 7254 is unusual, as the identity, 39 × 186 = 7254,
containing multiplicand, multiplier, and product is 1 through 9
pandigital.

Find the sum of all products whose multiplicand/multiplier/product
identity can be written as a 1 through 9 pandigital.  HINT: Some
products can be obtained in more than one way so be sure to only
include it once in your sum.

> module Main where

> import Prelude 
> import qualified System
> import qualified Data.List as List
> import qualified IO

Since the product must have at least as many digits as the 
sum of the digits of the terms - 1, we only need to consider 
numbers up to 9999.  While crude, this should be fast enough.

> pandigital :: (Int, Int) -> Bool
> pandigital (n, m) = 
>   let ds = show n ++ show m ++ show (n * m) in
>   List.sort ds == "123456789"

1 is not needed

> pandigitals :: [(Int, Int, Int)]
> pandigitals = 
>   let n = 9999
>       ls = map (\x -> [(x, y) | y <- [x .. n]]) [2 .. n]
>       f (l @ ((x, _):_)) =
>         let k = length (show x)
>             k' = 9 - k
>             l' = List.takeWhile (\(_, y) -> length (show y ++ show (x * y)) <= k') l 
>             l'' = List.filter pandigital l' in
>         map (\(x, y) -> (x, y, x * y)) l'' in
>       concat $ map f ls

> pandigitalSum :: [(Int, Int, Int)] -> Int
> pandigitalSum = sum . List.nub . map (\(_, _, y) -> y) 

> main :: IO ()
> main = 
>   do args <- System.getArgs 
>      case args of
>       [] -> putStr ("Pandigital sum: " ++ show (pandigitalSum pandigitals))
>       _ -> print "Usage: Euler" 

$ time Euler
Pandigital sum: 45228
real	0m0.090s
user	0m0.082s
sys	0m0.006s
