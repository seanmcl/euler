
> module Euler where

> import Prelude
> import qualified Data.List as List
> import qualified Data.Edison.Coll.SplayHeap as Bag
> import qualified Data.Set as Set
> import qualified Data.Map as Map
> import qualified System.IO.Unsafe as Unsafe

> {-# LANGUAGE ScopedTypeVariables #-}

***** Problem 4 *****

A palindromic number reads the same both ways. The largest palindrome           
made from the product of two 2-digit numbers is 9009 = 91 × 99.

Find the largest palindrome made from the product of two 3-digit
numbers.

> palindrome :: Integer -> Bool
> palindrome n = 
>   let s = show n 
>       k = length s
>       k' = k `div` 2
>       s1 = take k' s
>       k'' = if k `mod` 2 == 0 then k' else k' + 1
>       s2 = reverse (drop k'' s) in
>   s1 == s2

> euler4 :: Integer
> euler4 = 
>   let pairs = [(n, m) | n <- [1 .. 999], m <- [n .. 999] ]
>       prods = map (uncurry (*)) pairs
>       l = reverse (List.sort prods) in
>   case List.find palindrome l of
>     Nothing -> error "Impossible"
>     Just n -> n

***** Problem 5 *****

2520 is the smallest number that can be divided by each of the numbers
from 1 to 10 without any remainder.

What is the smallest number that is evenly divisible by all of the
numbers from 1 to 20?

Well, certainly a lower bound is 2 * 3 * 5 * 7 * 11 * 13 * 17 * 19

We also need at least 4 powers of 2 and 2 powers of 3.

so how about 16 * 9 * 5 * 7 * 11 * 13 * 17 * 19 = 232792560?

***** Problem 6 *****

The sum of the squares of the first ten natural numbers is, 1^(2) +
2^(2) + ... + 10^(2) = 385

The square of the sum of the first ten natural numbers is, (1 + 2 +
... + 10)^(2) = 55^(2) = 3025

Hence the difference between the sum of the squares of the first ten
natural numbers and the square of the sum is 3025 − 385 = 2640.

Find the difference between the sum of the squares of the first one
hundred natural numbers and the square of the sum.

> square :: Integer -> Integer
> square x = x * x

> euler6 :: Integer
> euler6 = square (sum [1 .. 100]) - sum (map square [1 .. 100]) 

***** Problem 7 *****

By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can
see that the 6^(th) prime is 13.

What is the 10001^(st) prime number?

OK, try brute force...

> nthPrime :: Int -> Integer
> nthPrime n = primes !! (n - 1)

> euler7a :: Integer
> euler7a = nthPrime 10001

Not happening...

One could always use Mathematica

In[4]:= Prime[10001]

Out[4]= 104743

But that's a bit unfair.  How does it come up with the solution so
fast?  Perhaps it stores the first million or so.  

Still, Sqrt(104743) = 323, so we could just check the primes up to 323.

> nthPrime2 :: Integer -> Integer -> Integer
> nthPrime2 0 n = 
>   if isPrime n then n
>   else nthPrime2 0 (n+1)
> nthPrime2 k n = 
>   if isPrime n then nthPrime2 (k-1) (n+1)
>   else nthPrime2 k (n+1)

> euler7 :: Integer
> euler7 = nthPrime2 10001 1

Great!

~/save/versioned/projects/euler
$ time Euler
104743

real	0m0.160s
user	0m0.151s
sys	0m0.006s

***** Problem 8 *****

Find the greatest product of five consecutive digits in the 1000-digit
number.

73167176531330624919225119674426574742355349194934
96983520312774506326239578318016984801869478851843
85861560789112949495459501737958331952853208805511
12540698747158523863050715693290963295227443043557
66896648950445244523161731856403098711121722383113
62229893423380308135336276614282806444486645238749
30358907296290491560440772390713810515859307960866
70172427121883998797908792274921901699720888093776
65727333001053367881220235421809751254540594752243
52584907711670556013604839586446706324415722155397
53697817977846174064955149290862569321978468622482
83972241375657056057490261407972968652414535100474
82166370484403199890008895243450658541227588666881
16427171479924442928230863465674813919123162824586
17866458359124566529476545682848912883142607690042
24219022671055626321111109370544217506941658960408
07198403850962455444362981230987879927244284909188
84580156166097919133875499200524063689912560717606
05886116467109405077541002256983155200055935729725
71636269561882670428252483600823257530420752963450

> int :: Char -> Integer
> int n = read [n] :: Integer

> prod :: [Integer] -> [Integer]
> prod (x1:x2:x3:x4:x5:l) = x1*x2*x3*x4*x5 : prod (x2:x3:x4:x5:l)
> prod _ = []

> euler8 :: Integer
> euler8 = 
>   let s = "73167176531330624919225119674426574742355349194934\
>           \96983520312774506326239578318016984801869478851843\
>           \85861560789112949495459501737958331952853208805511\
>           \12540698747158523863050715693290963295227443043557\
>           \66896648950445244523161731856403098711121722383113\
>           \62229893423380308135336276614282806444486645238749\
>           \30358907296290491560440772390713810515859307960866\
>           \70172427121883998797908792274921901699720888093776\
>           \65727333001053367881220235421809751254540594752243\
>           \52584907711670556013604839586446706324415722155397\
>           \53697817977846174064955149290862569321978468622482\
>           \83972241375657056057490261407972968652414535100474\
>           \82166370484403199890008895243450658541227588666881\
>           \16427171479924442928230863465674813919123162824586\
>           \17866458359124566529476545682848912883142607690042\
>           \24219022671055626321111109370544217506941658960408\
>           \07198403850962455444362981230987879927244284909188\
>           \84580156166097919133875499200524063689912560717606\
>           \05886116467109405077541002256983155200055935729725\
>           \71636269561882670428252483600823257530420752963450" 
>       s' = map int s in
>   maximum (prod s')

***** Problem 9 *****

A Pythagorean triplet is a set of three natural numbers, a < b < c,
for which, a^(2) + b^(2) = c^(2)

For example, 3^(2) + 4^(2) = 9 + 16 = 25 = 5^(2).

There exists exactly one Pythagorean triplet for which a + b + c =
1000.  Find the product abc.

-- > euler9 :: Integer

> euler9 :: Integer
> euler9 =
>   let l = [(a, b, c) | a <- [1 .. 1000], b <- [a + 1 .. 1000], c <- [b + 1 .. 1000]] in
>   case List.find (\(a, b, c) -> square a + square b == square c && a + b + c == 1000) l of
>     Nothing -> error "Impossible" 
>     Just (a, b, c) -> a * b * c

~/save/versioned/projects/euler
$ time Euler
31875000

real	0m13.504s
user	0m13.146s
sys	0m0.149s

***** Problem 10 *****

The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.

Find the sum of all the primes below two million.

Once again, try the simplest thing that could possibly work.

> euler10 :: Integer
> euler10 = foldr (\n sum -> if isPrime n then sum + n else sum) 0 
>           (reverse [2 .. 2000000])

~/save/versioned/projects/euler
$ time Euler
142913828922

real	0m29.811s
user	0m28.900s
sys	0m0.326s

Respectable

***** Problem 11 *****

In the 20×20 grid below, four numbers along a diagonal line have been
marked in red.

08 02 22 97 38 15 00 40 00 75 04 05 07 78 52 12 50 77 91 08
49 49 99 40 17 81 18 57 60 87 17 40 98 43 69 48 04 56 62 00
81 49 31 73 55 79 14 29 93 71 40 67 53 88 30 03 49 13 36 65
52 70 95 23 04 60 11 42 69 24 68 56 01 32 56 71 37 02 36 91
22 31 16 71 51 67 63 89 41 92 36 54 22 40 40 28 66 33 13 80
24 47 32 60 99 03 45 02 44 75 33 53 78 36 84 20 35 17 12 50
32 98 81 28 64 23 67 10 26 38 40 67 59 54 70 66 18 38 64 70
67 26 20 68 02 62 12 20 95 63 94 39 63 08 40 91 66 49 94 21
24 55 58 05 66 73 99 26 97 17 78 78 96 83 14 88 34 89 63 72
21 36 23 09 75 00 76 44 20 45 35 14 00 61 33 97 34 31 33 95
78 17 53 28 22 75 31 67 15 94 03 80 04 62 16 14 09 53 56 92
16 39 05 42 96 35 31 47 55 58 88 24 00 17 54 24 36 29 85 57
86 56 00 48 35 71 89 07 05 44 44 37 44 60 21 58 51 54 17 58
19 80 81 68 05 94 47 69 28 73 92 13 86 52 17 77 04 89 55 40
04 52 08 83 97 35 99 16 07 97 57 32 16 26 26 79 33 27 98 66
88 36 68 87 57 62 20 72 03 46 33 67 46 55 12 32 63 93 53 69
04 42 16 73 38 25 39 11 24 94 72 18 08 46 29 32 40 62 76 36
20 69 36 41 72 30 23 88 34 62 99 69 82 67 59 85 74 04 36 16
20 73 35 29 78 31 90 01 74 31 49 71 48 86 81 16 23 57 05 54
01 70 54 71 83 51 54 69 16 92 33 48 61 43 52 01 89 19 67 48

The product of these numbers is 26 × 63 × 78 × 14 = 1788696.

What is the greatest product of four adjacent numbers in any direction
(up, down, left, right, or diagonally) in the 20×20 grid?

Let's see.  Efficiency will not be an issue here.  We Just want a
clean solution.  We'll have rows, columns, and diagnoals.  Figuring
out the diagonals will be the tricky part.  We'll store the
matrix in row major order.

Products of sublists of length 4 in l. 

> subs :: [Integer] -> [Integer]
> subs (x1:x2:x3:x4:l) = x1 * x2 * x3 * x4 : subs (x2:x3:x4:l)
> subs _ = []

Column entries

> cols :: [[Integer]] -> [[Integer]]
> cols mat = 
>   let n = length (head mat) in
>   map (col mat) [0 .. n - 1]

> col :: [[Integer]] -> Int -> [Integer]
> col mat k = map (!! k) mat

Diagonal from (i, 0) to (0, i)

> diags :: [[Integer]] -> [[Integer]]
> diags mat = 
>   let n = length (head mat) in
>   map (diag mat) [0 .. n - 1]

> diag :: [[Integer]] -> Int -> [Integer]
> diag mat n = map (\(i, j) -> mat !! i !! j) (coords n)

> coords :: Int -> [(Int, Int)]
> coords n = coords1 n 0

> coords1 :: Int -> Int -> [(Int, Int)] 
> coords1 0 n = [(0, n)]
> coords1 k n = (k, n) : coords1 (k-1) (n+1) 

Parser

> chop :: Int -> [a] -> [[a]]
> chop _n [] = []
> chop n l = take n l : chop n (drop n l)

> parse11 :: String -> [[Integer]]
> parse11 s = chop 20 (map read (chop 3 s))

-- > parse11 s =  (map read (chop 3 s))

All strips

> euler11 :: Integer
> euler11 =
>   let str = "08 02 22 97 38 15 00 40 00 75 04 05 07 78 52 12 50 77 91 08 \
>             \49 49 99 40 17 81 18 57 60 87 17 40 98 43 69 48 04 56 62 00 \
>             \81 49 31 73 55 79 14 29 93 71 40 67 53 88 30 03 49 13 36 65 \
>             \52 70 95 23 04 60 11 42 69 24 68 56 01 32 56 71 37 02 36 91 \
>             \22 31 16 71 51 67 63 89 41 92 36 54 22 40 40 28 66 33 13 80 \
>             \24 47 32 60 99 03 45 02 44 75 33 53 78 36 84 20 35 17 12 50 \
>             \32 98 81 28 64 23 67 10 26 38 40 67 59 54 70 66 18 38 64 70 \
>             \67 26 20 68 02 62 12 20 95 63 94 39 63 08 40 91 66 49 94 21 \
>             \24 55 58 05 66 73 99 26 97 17 78 78 96 83 14 88 34 89 63 72 \
>             \21 36 23 09 75 00 76 44 20 45 35 14 00 61 33 97 34 31 33 95 \
>             \78 17 53 28 22 75 31 67 15 94 03 80 04 62 16 14 09 53 56 92 \
>             \16 39 05 42 96 35 31 47 55 58 88 24 00 17 54 24 36 29 85 57 \
>             \86 56 00 48 35 71 89 07 05 44 44 37 44 60 21 58 51 54 17 58 \
>             \19 80 81 68 05 94 47 69 28 73 92 13 86 52 17 77 04 89 55 40 \
>             \04 52 08 83 97 35 99 16 07 97 57 32 16 26 26 79 33 27 98 66 \
>             \88 36 68 87 57 62 20 72 03 46 33 67 46 55 12 32 63 93 53 69 \
>             \04 42 16 73 38 25 39 11 24 94 72 18 08 46 29 32 40 62 76 36 \
>             \20 69 36 41 72 30 23 88 34 62 99 69 82 67 59 85 74 04 36 16 \
>             \20 73 35 29 78 31 90 01 74 31 49 71 48 86 81 16 23 57 05 54 \
>             \01 70 54 71 83 51 54 69 16 92 33 48 61 43 52 01 89 19 67 48 "
>       mat = parse11 str 
>       cs = cols mat
>       dg1 = diags mat
>       dg2 = diags (map reverse mat) 
>       dg3 = diags (reverse mat)
>       dg4 = diags (reverse (map reverse mat))
>       prods = concat (map subs (concat [mat, cs, dg1, dg2, dg2, dg3, dg4])) in
>   maximum prods

***** Problem 12 *****

The sequence of triangle numbers is generated by adding the natural
numbers. So the 7^(th) triangle number would be 1 + 2 + 3 + 4 + 5 + 6
+ 7 = 28. The first ten terms would be:

1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...

Let us list the factors of the first seven triangle numbers:

     1: 1
     3: 1,3
     6: 1,2,3,6
    10: 1,2,5,10
    15: 1,3,5,15
    21: 1,3,7,21
    28: 1,2,4,7,14,28

We can see that 28 is the first triangle number to have over five
divisors.

What is the value of the first triangle number to have over five
hundred divisors?

> triangle :: Integer -> Integer
> triangle n = n * (n+1) `div` 2

> triangles :: [Integer]
> triangles = map triangle [1 ..]

> primeDivisors :: Integer -> Bag.Heap Integer
> primeDivisors n = 
>   case firstFactor n of
>     Nothing -> Bag.singleton n
>     Just k -> Bag.union (primeDivisors k) (primeDivisors (n `div` k))

> numDivisors :: Integer -> Int
> numDivisors n = 
>   if n == 1 then 1 else
>   let pds = primeDivisors n 
>       (num, _) = Bag.foldr (\x (prod, seen) -> if List.elem x seen then (prod, seen) else ((Bag.count x pds + 1) * prod, x:seen)) (1, []) pds in
>   num

> euler12 :: Integer
> euler12 = 
>   case List.find (\n -> numDivisors n > 500) triangles of
>     Nothing -> error "Impossible" 
>     Just n -> n

***** Problem 13 *****
