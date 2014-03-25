
The following iterative sequence is defined for the set of positive
integers:

n -> n/2 (n is even)
n -> 3n + 1 (n is odd)

Using the rule above and starting with 13, we generate the following
sequence:

13 -> 40 -> 20 -> 10 -> 5 -> 16 -> 8 -> 4 -> 2 -> 1

It can be seen that this sequence (starting at 13 and finishing at 1)
contains 10 terms. Although it has not been proved yet (Collatz
Problem), it is thought that all starting numbers finish at 1.

Which starting number, under one million, produces the longest chain?

NOTE: Once the chain starts the terms are allowed to go above one
million.

> module Main where

> import Prelude 
> import qualified List
> import qualified Data.Map as Map

> collatz :: Integer -> [Integer]
> collatz 1 = [1]
> collatz n | n `mod` 2 == 0 = n : collatz (n `div` 2)
>           | otherwise = n : collatz (3 * n + 1)

> collatz' :: Integer -> Integer
> collatz' 1 = 1
> collatz' n | n `mod` 2 == 0 = 1 + collatz' (n `div` 2)
>            | otherwise = 1 + collatz' (3 * n + 1)

> collatz1 :: Integer -> Integer -> Integer
> collatz1 1 acc = acc
> collatz1 n acc | n `mod` 2 == 0 = collatz1 (n `div` 2) (acc+1)
>                | otherwise = collatz1 (3 * n + 1) (acc+1)

Try brute force

> euler14a :: Integer
> euler14a = iter 999999 (\n acc -> let k = collatz1 n 1 in max k acc) 0

> type Table = Map.Map Integer Integer

> collatzT :: (Integer, Table) -> (Integer, Table)
> collatzT (1, t) = (1, t)
> collatzT (n, t) =
>   case Map.lookup n t of
>     Just k -> (k, t)
>     Nothing ->  
>       let (k, t') = if n `mod` 2 == 0 
>                     then collatzT (n `div` 2, t)
>                     else collatzT (3 * n + 1, t) in
>       (k+1, Map.insert n (k+1) t')

> iter :: Integer -> (Integer -> a -> a) -> a -> a
> iter 0 _f x = x
> iter n f x = f n (iter (n-1) f x)

> iter' :: Integer -> Integer -> (Integer -> a -> a) -> a -> a
> iter' n m _ x | n == m = x
> iter' n m f x = iter' n (m-1) f (f m x)

> euler14b :: Integer
> euler14b = 
>   let (v, _, _) = iter 999999
>                  (\n (v, max, t) -> 
>                     let (k, t') = collatzT(n, t) in
>                     if k > max then (n, k, t') else (v, max, t')) 
>                  (0, 0, Map.empty) in
>   v

> mainb :: IO ()
> mainb = print ("The longest chain under 10^6 is " 
>               ++ show euler14b)

~/save/versioned/projects/compiler/euler/14
$ time Euler +RTS -K100000000
"The longest chain under 10^6 is 837799"

real	0m57.327s
user	0m56.084s
sys	0m0.732s

Mmm... That's pushing the 1 minute barrier.  Not satisfied.
