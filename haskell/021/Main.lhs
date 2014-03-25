
Let d(n) be defined as the sum of proper divisors of n (numbers less
than n which divide evenly into n).  If d(a) = b and d(b) = a, where 
a ≠ b, then a and b are an amicable pair and each of a and b are called
amicable numbers.

For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 11, 20,
22, 44, 55 and 110; therefore d(220) = 284. The proper divisors of 284
are 1, 2, 4, 71 and 142; so d(284) = 220.

Evaluate the sum of all the amicable numbers under 10000.

> module Main where

> import Prelude 
> import qualified System
> import qualified IO
> import qualified Data.List as List
> import qualified Maybe
> import qualified EulerLib as Lib

> factorSum :: Integer -> Integer
> factorSum = sum . Lib.properDivisors

> amicablePair :: Integer -> Maybe Integer 
> amicablePair n = 
>   let k = factorSum n
>       k' = factorSum k in
>   if k' == n && k /= n
>     then Just k 
>   else Nothing 

> amicable :: Integer -> Bool
> amicable = Maybe.isJust . amicablePair

> p21 :: Integer -> Integer
> p21 n = sum (filter amicable [1 .. n])

> main :: IO ()
> main = 
>   do args <- System.getArgs 
>      case args of
>       [] -> print "Usage: Euler <int>" 
>       h : _ -> putStr ("Sum of the amicable numbers up to " ++ show h ++
>                        " is " ++ show (p21 (read h)))

$ time Euler 10000
Sum of the amicable numbers up to "10000" is 31626
real	0m0.454s
user	0m0.439s
sys	0m0.008s
