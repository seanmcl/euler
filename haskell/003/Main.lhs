
The prime factors of 13195 are 5, 7, 13 and 29.

What is the largest prime factor of the number 600851475143 ?

> module Main where

> import Prelude
> import qualified System.Environment as Env
> import qualified EulerLib as Lib

Try brute force search.

> brute :: Integer -> Integer
> brute n =
>   let sn = Lib.squareRoot n
>       smallPrimes = takeWhile (\x -> x <= sn) Lib.primes
>       factors = filter (\x -> n `mod` x == 0) smallPrimes in
>   last factors

This works, but we can do better.

$ time ./Euler 600851475143
Biggest prime factor of "600851475143" is 6857
real	0m1.002s
user	0m0.978s
sys	0m0.012s

How about, when you find a factor, run the process recursively...

> bigFactor :: Integer -> Integer
> bigFactor n =
>   case Lib.firstFactor n of
>     Nothing -> n
>     Just k -> max (bigFactor k) (bigFactor (n `div` k))

> main :: IO ()
> main =
>   do args <- Env.getArgs
>      case args of
>       [] -> print "Usage: Euler <int>"
>       h : _ -> putStr ("Biggest prime factor of " ++ show h ++
>                        " is " ++ show (brute (read h)))

~/save/versioned/projects/compiler/euler/3
$ time Euler 600851475143
Biggest prime factor of "600851475143" is 6857
real	0m0.079s
user	0m0.072s
sys	0m0.006s
