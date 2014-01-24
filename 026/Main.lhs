
A unit fraction contains 1 in the numerator. The decimal
representation of the unit fractions with denominators 2 to 10 are
given:

    ^(1)/_(2)	= 	0.5
    ^(1)/_(3)	= 	0.(3)
    ^(1)/_(4)	= 	0.25
    ^(1)/_(5)	= 	0.2
    ^(1)/_(6)	= 	0.1(6)
    ^(1)/_(7)	= 	0.(142857)
    ^(1)/_(8)	= 	0.125
    ^(1)/_(9)	= 	0.(1)
    ^(1)/_(10)	= 	0.1

Where 0.1(6) means 0.166666..., and has a 1-digit recurring cycle. It
can be seen that ^(1)/_(7) has a 6-digit recurring cycle.

Find the value of d < 1000 for which ^(1)/_(d) contains the longest
recurring cycle in its decimal fraction part.

> module Main where

A cycle for 1/d can't be longer than (d-1).  This can be seen
by examining the long division algorithm.  After each subtraction,
the number must be different to avoid a cycle.  This can happen
d-1 ways.  So we can simply do the division with exact arithmetic
and look at the first d-1 digits.  Now the question is, if there
is a cycle, how do you detect where it begins and ends?

> import Prelude 
> import qualified IO
> import qualified Data.List as List
> import qualified Data.Maybe as Maybe
> import qualified System

From MathWorld:

When a rational number m/n with (m,n)=1 is expanded, the period begins
after s terms and has length t, where s and t are the smallest numbers
satisfying 10^s = 10^(s+t) (mod n).

As an example, consider n=84.

10^0 = 1 
10^1 = 10
10^2 = 16 
10^3 = -8
10^4 = 4 
10^5 = 40 
10^6 = -20 
10^7 = -32
10^8 = 16

so s=2, t=6. The decimal representation is 1/84 = 0.0(1190476). 

> firstPeriod :: Eq a => [a] -> a
> firstPeriod l = first l []
>   where first (h:t) l = if elem h l then h else first t (h:l)
>         first [] _ = error "Impossible" 

period n ---> (s, t)

> period :: Integer -> (Int, Int)
> period n = 
>   let pows = map (\x -> (10 ^ x) `mod` n) [0 ..] 
>       k = firstPeriod pows 
>       s = Maybe.fromJust $ List.elemIndex k pows
>       t = 1 + Maybe.fromJust (List.elemIndex k (drop (s+1) pows)) in
>   (s, t)

Now find the index that leads => the longest period.

> longest :: Integer -> Integer
> longest n = snd $ maximum $ map (\x -> (snd (period x), x)) [1 .. n]

> main :: IO ()
> main = 
>   do args <- System.getArgs 
>      case args of
>       [] -> print "Usage: Euler <int>" 
>       n : _ -> putStr ("Longest cycle up to " ++ n ++ " is " ++ show (longest (read n)))

$ time Euler 1000
Longest cycle up to 1000 is 983
real	0m0.954s
user	0m0.930s
sys	0m0.012s

