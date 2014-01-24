
For an integer n >= 4, we define the lower prime square root of n,
denoted by lps(n), as the largest prime <= Sqrt[n] and the upper prime
square root of n, ups(n), as the smallest prime >= Sqrt[n].

So, for example, lps(4) = 2 = ups(4), lps(1000) = 31, ups(1000) = 37.
Let us call an integer n >= 4 semidivisible, if one of lps(n) and
ups(n) divides n, but not both.

The sum of the semidivisible numbers not exceeding 15 is 30, the
numbers are 8, 10 and 12.  15 is not semidivisible because it is a
multiple of both lps(15) = 3 and ups(15) = 5.  As a further example,
the sum of the 92 semidivisible numbers up to 1000 is 34825.

What is the sum of all semidivisible numbers not exceeding
999966663333 ?

> module Main where

> import Prelude 
> import qualified Data.List as List
> import qualified System
> import qualified IO
> import qualified EulerLib as Lib

> type Square = (Integer, Integer)

> primeSquares :: [Square]
> primeSquares = map (\p -> (p, p * p)) Lib.primes

Find all the semidivisible numbers between the squares
of two primes.

> test1 :: Square -> Square -> [Integer]
> test1 (p1, p12) (p2, p22) = 
>   let l1 = [ p12 + p1, p12 + 2 * p1 .. | i <- [1 .. limit], f i `mod` p2 /= 0]
>         where f i = p12 + p1 * i
>               limit = (p22 - p12) `div` p1
>       l2 = [ g i | i <- [1 .. limit], g i `mod` p1 /= 0] 
>         where g i = p22 - p2 * i 
>               limit = (p22 - p12) `div` p2 in
>   List.sort (l1 ++ l2)

> semidivisible :: [Integer]
> semidivisible = semi primeSquares
>   where semi (h1:h2:l) = test1 h1 h2 ++ semi (h2:l)
>         semi _ = error "Impossible" 

> semiSum :: Integer -> Integer
> semiSum n = sum $ takeWhile (< n) semidivisible

> main :: IO ()
> main = do 
>   args <- System.getArgs 
>   case args of
>     [] -> print "Usage: Euler <file>" 
>     n : _ -> putStr ("Sum: " ++ show (semiSum (read n)))

$ time Euler 999966663333 +RTS -K100000000 
Sum: 1259187438574927161
real	0m27.199s
user	0m25.846s
sys	0m0.778s

With Data.Numbers.Primes it's slightly faster

$ time Euler 999966663333 +RTS -K1000000000000
Sum: 1259187438574927161
real	0m23.824s
user	0m22.533s
sys	0m0.756s

Funktio's solution is much better, though I don't see why mine is
exhausting the stack.
