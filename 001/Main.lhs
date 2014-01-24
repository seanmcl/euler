
If we list all the natural numbers below 10 that are multiples of 3 or
5, we get 3, 5, 6 and 9. The sum of these multiples is 23.

Find the sum of all the multiples of 3 or 5 below 1000.

> module Main where

> import Prelude
> import qualified System
> import qualified IO

> legal :: Integer -> Bool
> legal n = n `mod` 3 == 0 || n `mod` 5 == 0

> euler1 :: Integer -> Integer
> euler1 n = sum (filter legal [1 .. n])

> main :: IO ()
> main = 
>   do args <- System.getArgs 
>      case args of
>       [] -> print "Usage: Euler <int>" 
>       h : _ -> putStr ("Sum up to " ++ show h ++
>                        " is " ++ show (euler1 (read h)))

~/save/versioned/projects/compiler/euler/1
$ Euler 999
Sum up to "999" is 233168
