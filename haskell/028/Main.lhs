
Starting with the number 1 and moving to the right in a clockwise
direction a 5 by 5 spiral is formed as follows:

21 22 23 24 25
20  7  8  9 10
19  6  1  2 11
18  5  4  3 12
17 16 15 14 13

It can be verified that the sum of both diagonals is 101.

What is the sum of both diagonals in a 1001 by 1001 spiral formed in
the same way?

> module Main where

One solution would be to find a closed form for the solution
by analtyically finding the numbers on the diagonal.  I decided
to write out the spiral matrix.

> import Prelude 
> import qualified Data.List as List
> import qualified System

> downto :: Int -> Int -> [Int]
> downto n k = reverse [n - k + 1 .. n]

> upto :: Int -> Int -> [Int]
> upto n k = [n .. n + k - 1]

> descend :: Int -> [Int]
> descend n = n : descend (n-1)

> chop :: Int -> [a] -> [[a]]
> chop n l = take n l : chop n (drop n l)

Enumerate the elements of the spiral matrix.  (i, j, n)
i and j are the coordinates, n is the value.

> enumPairs :: Int -> Int -> Int -> [(Int, Int, Int)]
> enumPairs _ x 1 = [(x, x, 1)]
> enumPairs dim x start = 
>   if start == 0 then [] else 
>   let d1 = dim - 1
>       lt:ll:lb:lr:(start':_):_ = chop d1 (descend start)
>       md = downto x d1
>       mu = upto (x - d1) d1 
>       top = zip3 md
>                  (repeat x)
>                  lt
>       left = zip3 (repeat (x - dim + 1))
>                   md
>                   ll
>       bot = zip3 mu
>                  (repeat (x - dim + 1))
>                  lb
>       right = zip3 (repeat x)
>                    mu
>                    lr in
>   top ++ left ++ bot ++ right ++ enumPairs (dim-2) (x-1) start'

> pairs :: Int -> [(Int, Int, Int)]
> pairs n = enumPairs n (n-1) (n^2)

> diagSum :: Int -> Int
> diagSum n = 
>   let ps = pairs n 
>       foldFn (x, y, k) s = 
>         if x == y || x + y == (n - 1) then k + s else s in
>   foldr foldFn 0 ps

> main :: IO ()
> main = 
>   do args <- System.getArgs 
>      case args of
>       n : _ -> putStr ("Diag sum: " ++ show(diagSum $ read n))
>       _ -> print "Usage: Euler <int>" 

~/save/versioned/projects/compiler/euler/028
$ time Euler 1001
Diag sum: 669171001
real	0m0.617s
user	0m0.597s
sys	0m0.013s

