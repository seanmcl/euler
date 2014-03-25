
2^(15) = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.

What is the sum of the digits of the number 2^(1000)?

> module Main where

> import Prelude 
> import qualified System

> sumDigits :: Integer -> Integer 
> sumDigits n = sum (map (read . list) (show n))
>   where list x = [x]

> power :: Integer -> Integer -> Integer
> power _n 0 = 1
> power n k | k `mod` 2 == 0 = let x = power n (k `div` 2) in
>                                      x * x
>           | otherwise = n * power n (k - 1)

> sumPow :: Integer -> Integer
> sumPow = sumDigits . power 2

> main :: IO ()
> main = 
>   do args <- System.getArgs 
>      case args of
>       [] -> print "Usage: Euler n" 
>       h : _ -> (print . show . sumPow . read) h

~/save/versioned/projects/compiler/euler/16
$ time Euler 1000
"1366"

real	0m0.006s
user	0m0.003s
sys	0m0.003s
