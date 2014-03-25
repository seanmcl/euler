
We shall call a positive integer A an "Alexandrian integer", if there
exist integers p, q, r such that: A = p q r and
1/A = 1/p + 1/q + 1/r

For example, 630 is an Alexandrian integer p = 5, q = -7, r = -18.
In fact, 630 is the 6^(th) Alexandrian integer, the first 6
Alexandrian integers being: 6, 42, 120, 156, 420 and 630.

Find the 150000^(th) Alexandrian integer.

> module Main where

> import Prelude 
> import qualified Data.Maybe as Maybe
> import qualified Data.List as List
> import qualified System
> import qualified IO

 import qualified EulerLib as Lib

 import qualified Data.List as List

Note the equations

A = p q r
1/A = 1/p + 1/q + 1/r

is equivalent to the single equation

pr + qr + pq = 1

Notice also that if p,q,r is a solution to the above, then
so is -p, -q, -r.  Since at least one of p, q, r must be positive and
one must be negative, we can assume that exactly two of them are
positive.  Suppose those are p and q.  We can then solve the equation 
for r:

r = (1 - pq) / (p + q)

so |r| < min(p,q)

> alex3_1 :: [(Integer, Integer, Integer)]
> alex3_1 = Maybe.mapMaybe mapFn [(i, j) | j <- [2 .. ], i <- [2 .. j]]
>   where mapFn (p, q) = if p * q `mod` (p + q) == 1 
>                        then Just (p, q, (1 - p * q) `div` (p + q))
>                        else Nothing

> alex_1 :: [Integer]
> alex_1 = map (\(p, q, r) -> -p * q * r) alex3_1

This solution is bad for a number of reasons.  It's slow, and the numbers
aren't in order.

An equivalent formulation is to find solutions of the equation

pq = 1 (p + q)

Changing variables, n = p + q, this is equivalent =>

p(n - p) = 1 (n), equivalent =>
-p^2 = 1 (n) or

p^2 = -1 (n).

This is true if and only if -1 is a quadratic residue mod n.  Using
Legendre/Jacobi/Kronecker notation, 

(-1 \ n) = 1

We can calculate (-1 \ n) using the Kroneker generalization.

(-1 \ n) = -1^((n-1)/2) if n is odd.  If n is even, we can just
factor out the powers of 2 since -1 = -1 mod 8.

jacobi n calculates (-1 \ n)

Note that if jacobi n = 1, it not guaranteed that there is a 
residue since n can be composite.

if p^2 = -1 (n) then so is (n-p)^2 = -1 (n), so we only need to
scan half the possibilities.

> halfResidues m = List.filter f [1 .. m `div` 2] 
>   where f n = (n * n) `mod` m == m - 1

> alexes n = map f (halfResidues n)
>   where f p = (p, q, r, p * q * r) 
>           where q = n - p
>                 r = (1 - p * q) `div` n 

> jacobi :: Integer -> Integer
> jacobi n = if odd n 
>              then if n `mod` 4 == 1 then 1 else -1
>            else jacobi (n `div` 2)

> possibleResidue :: Integer -> Bool
> possibleResidue n = jacobi n == 1

> residue :: Integer -> Bool
> residue n = possibleResidue n && length (halfResidues n) > 0

> possibleResidues = filter possibleResidue [1 ..]

> residues = filter (\n -> length (halfResidues n) > 0) possibleResidues

> alex3 :: [(Integer, Integer, Integer)]
> alex3 = []

-- Maybe.mapMaybe mapFn [(p, n) | n <- [2 .. ], residue n, p <- [2 .. n `div` 2]]
-- >   where mapFn (p, n) = if residue n then if p * p `mod` n == n - 1 
-- >                        then Just (p, q, (1 - p * q) `div` (p + q))
-- >                        else Nothing

-- > alex :: [Integer]
-- > alex = map (\(p, q, r) -> p * q * r) alex3

> alex = List.concat (map alexes [2 ..])





> main :: IO ()
> main = 
>   do args <- System.getArgs 
>      case args of
>       [] -> print "Usage: Euler <int>" 
>       n : _ -> putStr (show (alex !! (read n)))
