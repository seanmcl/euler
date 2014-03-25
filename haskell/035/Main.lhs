
The number, 197, is called a circular prime because all rotations of
the digits: 197, 971, and 719, are themselves prime.

There are thirteen such primes below 100: 2, 3, 5, 7, 11, 13, 17, 31,
37, 71, 73, 79, and 97.

How many circular primes are there below one million?

> module Main where

> import Prelude 
> import qualified System
> import qualified IO
> import qualified EulerLib as Lib

> rotations :: [a] -> [[a]]
> rotations l = rot (length l) []
>   where rot 0 acc = acc
>         rot n acc = rot (n-1) (l':acc)
>           where l' = drop n l ++ take n l

> rotate :: Integer -> [Integer]
> rotate n = 
>   let s = show n 
>       ss = rotations s in
>   map read ss

We could note that if 123 is a circular prime, then so is
231 and 312 so we don't check the same number multiple times.
However, I think we can solve this without the complication.

> circularPrime :: Integer -> Bool
> circularPrime = all Lib.isPrime . rotate

Return the circular primes less than n.  

> circularPrimes :: Integer -> [Integer]
> circularPrimes n = filter circularPrime (takeWhile (< n) goodPrimes)

> main :: IO ()
> main = 
>   do args <- System.getArgs 
>      case args of
>       [] -> print "Usage: Euler <int>" 
>       h : _ -> putStr ("There are " ++ show (length $ circularPrimes $ read h)
>                        ++ " circular primes less than " ++ h)

$ time Euler 1000000
There are 55 circular primes less than 1000000
real	0m3.817s
user	0m3.722s
sys	0m0.042s

You could try removing primes with even numbers, but it doesn't help
very much.

> noEvens :: Integer -> Bool
> noEvens = not . any (flip elem "02468") . show

> goodPrimes :: [Integer]
> goodPrimes = filter noEvens Lib.primes

> circularPrimes1 :: Integer -> [Integer]
> circularPrimes1 n = 2 : filter circularPrime (takeWhile (< n) goodPrimes)

$ time Euler 1000000
There are 54 circular primes less than 1000000
real	0m3.117s
user	0m3.066s
sys	0m0.032s
