
Euler published the remarkable quadratic formula:

n² + n + 41

It turns out that the formula will produce 40 primes for the
consecutive values n = 0 to 39. However, when n = 40, 40^(2) + 40 + 41
= 40(40 + 1) + 41 is divisible by 41, and certainly when n = 41, 41² +
41 + 41 is clearly divisible by 41.

Using computers, the incredible formula n² − 79n + 1601 was
discovered, which produces 80 primes for the consecutive values n = 0
to 79. The product of the coefficients, −79 and 1601, is −126479.

Considering quadratics of the form:

    n² + an + b, where |a| < 1000 and |b| < 1000

    where |n| is the modulus/absolute value of n e.g. |11| = 11 and
    |−4| = 4

Find the product of the coefficients, a and b, for the quadratic
expression that produces the maximum number of primes for consecutive
values of n, starting with n = 0.

> module Main where

> import Prelude 
> import qualified System
> import qualified EulerLib as Lib

Find the longest run of primes with coefficients a and b.

> pairPrimes :: (Integer, Integer) -> Integer
> pairPrimes (a, b) = 
>   let ps = takeWhile Lib.isPrime (map mapFn [0 ..])
>       len = length ps in
>   toInteger len
>     where mapFn n = n^2 + a * n + b

Prelude.maximum gives a stack overflow.  Here's an optimized version.
*Main> maximum [1 .. 1000000]
maximum [1 .. 1000000]
*** Exception: stack overflow

> fastMax :: [(Integer, a)] -> (Integer, a)
> fastMax l = fast l (head l)
>   where fast [] m = m
>         fast ((h, x) : t) (m, y) = 
>           if h > m then fast t (h, x)
>           else fast t (m, y)

Find the optimal coefficients whose between the absolute value
upper bounds.

> optPair :: Integer -> Integer -> (Integer, Integer) 
> optPair absa absb =
>   let as = [-(absa-1) .. (absa-1)] 
>       posBs = takeWhile (< absb) Lib.primes
>       bs = map negate posBs ++ posBs
>       abs = Lib.allPairs as bs
>       runs = map (\ab -> (pairPrimes ab, ab)) abs
>       (_, ab) = fastMax runs in
>   ab

Multiply the coefficients.

> optVal :: Integer -> Integer -> Integer
> optVal absa absb = 
>   let (a, b) = optPair absa absb in
>   a * b

> main :: IO ()
> main = 
>   do args <- System.getArgs 
>      case args of
>       [a, b] -> putStr ("Optimum: " ++ show (optVal (read a) (read b)))
>       _ -> print "Usage: Euler <int> <int>" 

$ time Euler 1000 1000
Optimum: -59231
real	0m2.283s
user	0m2.230s
sys	0m0.028s

