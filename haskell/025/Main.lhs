
The Fibonacci sequence is defined by the recurrence relation:

    F_(n) = F_(n−1) + F_(n−2), where F_(1) = 1 and F_(2) = 1.

Hence the first 12 terms will be:

    F_(1) = 1
    F_(2) = 1
    F_(3) = 2
    F_(4) = 3
    F_(5) = 5
    F_(6) = 8
    F_(7) = 13
    F_(8) = 21
    F_(9) = 34
    F_(10) = 55
    F_(11) = 89
    F_(12) = 144

The 12th term, F_(12), is the first term to contain three digits.

What is the first term in the Fibonacci sequence to contain 1000
digits?

> module Main where

> import Prelude 
> import qualified IO
> import qualified Data.List as List
> import qualified System

> fibs :: [Integer]
> fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

> digits :: Int -> Int
> digits n = 
>   case List.findIndex (\k -> length (show k) == n) fibs of
>     Nothing -> error "Impossible" 
>     Just d -> d

> main :: IO ()
> main = 
>   do args <- System.getArgs 
>      case args of
>       [] -> print "Usage: Euler <int>" 
>       n : _ -> putStr ("First term with " ++ show n ++ " digits: " ++ (show $ digits $ read n))

$ time Euler 1000
First term with "1000" digits: 4782
real	0m0.179s
user	0m0.173s
sys	0m0.004s
