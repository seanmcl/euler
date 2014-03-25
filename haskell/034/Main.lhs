
145 is a curious number, as 1! + 4! + 5! = 1 + 24 + 120 = 145.

Find the sum of all numbers which are equal to the sum of the
factorial of their digits.

Note: as 1! = 1 and 2! = 2 are not sums they are not included.

> module Main where

There can be at most 7 digits, since 9! * 8 = 2903040.
So we only need to search up to 9! * 7 = 2540160.

> import Prelude 
> import qualified System
> import qualified Data.List as List
> import qualified Data.Char as Char

> fact :: Integer -> Integer
> fact 0 = 1
> fact n = n * fact (n-1)

> facts :: [Integer]
> facts = map fact [0 ..]

> curious :: Integer -> Bool
> curious n = 
>   let ns = map ((facts !!) . Char.digitToInt) (show n) in
>   n == sum ns

> curiousSum :: Integer
> curiousSum = sum (filter curious [3 .. (facts !! 9) * 7])

> main :: IO ()
> main = 
>   do args <- System.getArgs 
>      case args of
>       [] -> putStr ("Curious sum: " ++ show curiousSum)
>       _ -> print "Usage: Euler" 

$ time Euler
Curious sum: 40730
real	0m4.637s
user	0m4.539s
sys	0m0.050s
