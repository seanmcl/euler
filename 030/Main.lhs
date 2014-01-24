
Surprisingly there are only three numbers that can be written as the
sum of fourth powers of their digits:

    1634 = 1^(4) + 6^(4) + 3^(4) + 4^(4)
    8208 = 8^(4) + 2^(4) + 0^(4) + 8^(4)
    9474 = 9^(4) + 4^(4) + 7^(4) + 4^(4)

As 1 = 1^(4) is not a sum it is not included.

The sum of these numbers is 1634 + 8208 + 9474 = 19316.

Find the sum of all the numbers that can be written as the sum of
fifth powers of their digits.

> module Main where

> import Prelude 
> import qualified Data.Char as Char

Since 7 * 9 ^ 5 = 413343 has less than 7 digits, we need only 
consider 6 digit numbers.  We can loop through these easily.

> digitSum :: Int -> Int
> digitSum n = 
>   let ds :: [Int] = map Char.digitToInt (show n) in
>   sum (map (^5) ds)

> fifthSum :: Int
> fifthSum = sum $ filter (\n -> digitSum n == n) [2 .. 9^6]

> main :: IO ()
> main = putStr ("Fifth power sum: " ++ show fifthSum)

$ time Euler
Fifth power sum: 443839
real	0m2.778s
user	0m2.718s
sys	0m0.032s


