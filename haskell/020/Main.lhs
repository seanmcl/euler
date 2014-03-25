
Find the sum of the digits in the number 100!

> module Main where

> import Prelude 
> import qualified System
> import qualified IO

> fact :: Integer -> Integer
> fact 0 = 1
> fact n = n * fact (n-1)

> sumDigits :: Integer -> Integer
> sumDigits n = sum (map (read . (\x -> [x])) (show n))

> main :: IO ()
> main = 
>   do args <- System.getArgs 
>      case args of
>       [] -> print "Usage: Euler <int>" 
>       h : _ -> putStr (show h ++"! has " 
>                       ++ show (sumDigits (fact (read h))) ++ " digits\n")

~/save/versioned/projects/compiler/euler/20
$ time Euler 100
"100"! has 648 digits

real	0m0.005s
user	0m0.002s
sys	0m0.003s
