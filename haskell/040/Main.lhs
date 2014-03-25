An irrational decimal fraction is created by concatenating the
positive integers:

0.123456789101112131415161718192021...

It can be seen that the 12^(th) digit of the fractional part is 1.

If d_(n) represents the n^(th) digit of the fractional part, find the
value of the following expression.

d_(1) × d_(10) × d_(100) × d_(1000) × d_(10000) × d_(100000) × d_(1000000)

> module Main where

> import Prelude 
> import qualified Data.List as List
> import qualified Data.Char as Char

> bigDecimal :: [Int]
> bigDecimal = map Char.digitToInt $ List.concat (map show [1 ..])

> prod :: Int
> prod = product (map (\n -> bigDecimal !! (10 ^ n - 1)) [0 .. 6])

> main :: IO ()
> main = putStr ("Prod: " ++ show prod)


$ time Euler
Fifth power sum: 443839
real	0m2.778s
user	0m2.718s
sys	0m0.032s


