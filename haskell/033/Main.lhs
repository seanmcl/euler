
The fraction ^(49)/_(98) is a curious fraction, as an inexperienced
mathematician in attempting to simplify it may incorrectly believe
that ^(49)/_(98) = ^(4)/_(8), which is correct, is obtained by
cancelling the 9s.

We shall consider fractions like, ^(30)/_(50) = ^(3)/_(5), to be
trivial examples.

There are exactly four non-trivial examples of this type of fraction,
less than one in value, and containing two digits in the numerator and
denominator.

If the product of these four fractions is given in its lowest common
terms, find the value of the denominator.

> module Main where

> import Prelude 
> import qualified System
> import qualified Data.List as List
> import qualified Ratio
> import Ratio (Ratio, (%))

> curious :: (Int, Int) -> Bool
> curious (x, y) = 
>   let xy = x % y
>       x1 = x `div` 10
>       x2 = x `mod` 10
>       y1 = y `div` 10
>       y2 = y `mod` 10
>   in (x1 == y1 && y2 /= 0 && x2 % y2 == xy) ||
>      (x1 == y2 && y1 /= 0 && x2 % y1 == xy) ||
>      (x2 == y1 && y2 /= 0 && x1 % y2 == xy) ||
>      (x2 == y2 && y1 /= 0 && x1 % y1 == xy) 

> curiouses :: [(Int, Int)]
> curiouses = 
>   let f1 (x, y) = x /= y
>       f2 (x, y) = x `mod` 10 /= 0 || y `mod` 10 /= 0
>   in filter (\xy -> f1 xy && f2 xy && curious xy) $ [(x, y) | x <- [10 .. 99], y <- [x .. 99]]

> curiousProd :: Ratio Int
> curiousProd = foldr (\(x, y) p -> p * (x % y)) (1 % 1) curiouses

> main :: IO ()
> main = 
>   do args <- System.getArgs 
>      case args of
>       [] -> putStr ("Curious: " ++ show curiousProd)
>       _ -> print "Usage: Euler" 

$ time Euler
Curious: 1 % 100
real	0m0.018s
user	0m0.013s
sys	0m0.004s
