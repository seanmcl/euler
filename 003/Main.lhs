
The prime factors of 13195 are 5, 7, 13 and 29.

What is the largest prime factor of the number 600851475143 ?

> module Main where

> import Prelude
> import qualified System
> import qualified IO
> import qualified EulerLib as Lib

Try brute force search.

> euler3a :: Integer
> euler3a = 
>   let n = 600851475143 
>       sn = n `div` 2
>       smallPrimes = takeWhile (\x -> x <= sn) Lib.primes
>       factors = filter (\x -> n `mod` x == 0) smallPrimes in
>   maximum factors

Mmm... Not going to happen.  That's a big number!  

How about, when you find a factor, run the process recursively...

> bigFactor :: Integer -> Integer
> bigFactor n = 
>   case Lib.firstFactor n of
>     Nothing -> n
>     Just k -> max (bigFactor k) (bigFactor (n `div` k))

> main :: IO ()
> main = 
>   do args <- System.getArgs 
>      case args of
>       [] -> print "Usage: Euler <int>" 
>       h : _ -> putStr ("Biggest prime factor of " ++ show h ++
>                        " is " ++ show (bigFactor (read h)))

~/save/versioned/projects/compiler/euler/3
$ time Euler 600851475143
Biggest prime factor of "600851475143" is 6857
real	0m0.079s
user	0m0.072s
sys	0m0.006s




