
The number 3797 has an interesting property. Being prime itself, it is
possible to continuously remove digits from left to right, and remain
prime at each stage: 3797, 797, 97, and 7. Similarly we can work from
right to left: 3797, 379, 37, and 3.

Find the sum of the only eleven primes that are both truncatable from
left to right and right to left.

NOTE: 2, 3, 5, and 7 are not considered to be truncatable primes.

> module Main where

> import Prelude 
> import qualified System
> import qualified Data.List as List
> import qualified Data.Maybe as Maybe
> import qualified EulerLib as Lib

> extendL :: [Integer] -> [Integer]
> extendL = concat . iterate (List.sort . concatMap (\n -> Maybe.mapMaybe (extend n) [1, 2, 3, 5, 7, 9]))
>   where extend n k = 
>           let e = n + k * 10^(Lib.log10 n + 1) in
>           if Lib.isPrime e then Just e else Nothing 

> extendR :: [Integer] -> [Integer]
> extendR = concat . iterate (List.sort . concatMap (\n -> Maybe.mapMaybe (extend n) [1, 3, 7, 9]))
>   where extend n k = 
>           let e = 10 * n + k in
>           if Lib.isPrime e then Just e else Nothing 

> lefts :: [Integer]
> lefts = extendL [3, 7]

> rights :: [Integer]
> rights = extendR [2, 3, 5, 7]

> merge :: [Integer] -> [Integer] -> [Integer]
> merge (l1@(h1:t1)) (l2@(h2:t2)) =
>   if h1 == h2 then h1 : merge t1 t2
>   else if h1 < h2 then merge t1 l2
>   else merge l1 t2

> curious :: [Integer]
> curious = take 13 (merge lefts rights)

> main :: IO ()
> main = 
>   do args <- System.getArgs 
>      case args of
>       [] -> do putStr ("Curious: " ++ show curious ++ "\n")
>                putStr ("Sum: " ++ show (sum (drop 2 curious)))
>       _ -> print "Usage: Euler" 

$ time Euler
Curious: [3,7,23,37,53,73,313,317,373,797,3137,3797,739397]
Sum: 748317
real	0m0.045s
user	0m0.036s
sys	0m0.004s
