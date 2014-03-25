
The decimal number, 585 = 1001001001_(2) (binary), is palindromic in
both bases.

Find the sum of all numbers, less than one million, which are
palindromic in base 10 and base 2.

(Please note that the palindromic number, in either base, may not
include leading zeros.)

> module Main where

> import Prelude 
> import qualified System
> import qualified Data.Bits as Bits

> palindrome :: Eq a => [a] -> Bool
> palindrome l = l == reverse l

> binary :: Int -> [Bool]
> binary n = 
>   let l1 = map (Bits.testBit n) [0 .. 31]
>       l2 = dropWhile (== False) (reverse l1) 
>   in reverse l2

> curious :: Int -> Bool
> curious n = palindrome (show n) && palindrome (binary n)

> curiousSum :: Int
> curiousSum = sum (filter curious [1 .. 10^6])

> main :: IO ()
> main = 
>   do args <- System.getArgs 
>      case args of
>       [] -> putStr ("Curious sum: " ++ show curiousSum)
>       _ -> print "Usage: Euler" 

$ time Euler
Curious sum: 872187
real	0m0.390s
user	0m0.379s
sys	0m0.008s
