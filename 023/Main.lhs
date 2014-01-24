
A perfect number is a number for which the sum of its proper divisors
is exactly equal to the number. For example, the sum of the proper
divisors of 28 would be 1 + 2 + 4 + 7 + 14 = 28, which means that 28
is a perfect number.

A number whose proper divisors are less than the number is called
deficient and a number whose proper divisors exceed the number is
called abundant.

As 12 is the smallest abundant number, 1 + 2 + 3 + 4 + 6 = 16, the
smallest number that can be written as the sum of two abundant numbers
is 24. By mathematical analysis, it can be shown that all integers
greater than 28123 can be written as the sum of two abundant
numbers. However, this upper limit cannot be reduced any further by
analysis even though it is known that the greatest number that cannot
be expressed as the sum of two abundant numbers is less than this
limit.

Find the sum of all the positive integers which cannot be written as
the sum of two abundant numbers.

> module Main where

> import Prelude 
> import qualified IO
> import qualified Data.List as List
> import qualified EulerLib as Lib

> hi :: Integer
> hi = 28123

> abundant :: Integer -> Bool
> abundant n = sum (Lib.properDivisors n) > n

> abundants :: [Integer]
> abundants = filter abundant [1 ..]

> merge :: Ord a => [a] -> [a] -> [a]
> merge [] l = l
> merge l [] = l
> merge (l1@(h1:t1)) (l2@(h2:t2)) = 
>   if h1 == h2 then merge t1 l2 
>   else if h1 < h2 then h1 : merge t1 l2 
>   else h2 : merge l1 t2

> sums :: [Integer] -> [Integer]
> sums (h:t) = h + h : merge (map (h+) t) (sums t)
> sums _ = error "Impossible" 

> abundantSums :: [Integer]
> abundantSums = sums abundants

> diff :: Ord a => [a] -> [a] -> [a]
> diff [] _l = []
> diff l [] = l
> diff l1@(h1:t1) l2@(h2:t2) = 
>   if h1 == h2 then diff t1 l2
>   else if h1 < h2 then h1 : diff t1 l2
>   else diff l1 t2

> nonAbundantSums :: [Integer]
> nonAbundantSums = diff [1 .. hi] (takeWhile (<= hi) abundantSums)

> nonAbundantSum :: Integer
> nonAbundantSum = sum nonAbundantSums

> main :: IO ()
> main = 
>   putStr ("Sum: " ++ show nonAbundantSum)


