
If the numbers 1 to 5 are written out in words: one, two, three, four,
five, then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.

If all the numbers from 1 to 1000 (one thousand) inclusive were
written out in words, how many letters would be used?

NOTE: Do not count spaces or hyphens. For example, 342 (three hundred
and forty-two) contains 23 letters and 115 (one hundred and fifteen)
contains 20 letters. The use of "and" when writing out numbers is in
compliance with British usage.

> module Main where

> import Prelude 
> import qualified System

> letters1 :: Int -> String
> letters1 n = 
>   case n of 
>     0 -> ""
>     1 -> "one"
>     2 -> "two"
>     3 -> "three"
>     4 -> "four"
>     5 -> "five"
>     6 -> "six"
>     7 -> "seven"
>     8 -> "eight"
>     9 -> "nine"
>     10 -> "ten"
>     11 -> "eleven"
>     12 -> "twelve"
>     13 -> "thirteen"
>     14 -> "fourteen"
>     15 -> "fifteen"
>     16 -> "sixteen"
>     17 -> "seventeen"
>     18 -> "eighteen"
>     19 -> "nineteen"
>     20 -> "twenty"
>     30 -> "thirty"
>     40 -> "forty"
>     50 -> "fifty"
>     60 -> "sixty"
>     70 -> "seventy"
>     80 -> "eighty"
>     90 -> "ninety"
>     100 -> "hundred"
>     1000 -> "thousand"
>     _ -> error ("Bad int: " ++ show n)

> list :: a -> [a]
> list x = [x]

> lettersWord :: Int -> String
> lettersWord n = 
>   let chars = map (read . list) (reverse (show n)) in
>   case chars of
>     [n0] -> letters1 n0
>     [n0, n1] -> if n1 < 2 
>                   then letters1(10 * n1 + n0)
>                 else letters1 (10 * n1) ++ letters1 n0
>     [n0, n1, n2] -> letters1 n2 ++ letters1 100 
>                     ++ (if n0 + n1 > 0 then "and" else "")
>                     ++ lettersWord (10 * n1 + n0) 
>     [n0, n1, n2, n3] -> letters1 n3 ++ letters1 1000 
>                         ++ (if n0 + n1 + n2 > 0 then "and" else "")
>                         ++ lettersWord (100 * n2 + 10 * n1 + n0) 
>     _ -> error "Only can handle up to 9999"

> letters :: Int -> Int
> letters n = sum $ map (length . lettersWord) [1 .. n]

> main :: IO ()
> main = 
>   do args <- System.getArgs 
>      case args of
>       [] -> print "Usage: Euler n" 
>       h : _ -> (print . show . letters . read) h

~/save/versioned/projects/compiler/euler/17
$ time Euler 1000
"21124"

real	0m0.039s
user	0m0.034s
sys	0m0.004s
