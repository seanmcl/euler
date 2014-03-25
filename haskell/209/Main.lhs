
A k-input binary truth table is a map from k input bits (binary
digits, 0 [false] or 1 [true]) to 1 output bit. For example, the
2-input binary truth tables for the logical AND and XOR functions are:

x 	y 	x AND y
0	0	0
0	1	0
1	0	0
1	1	1
x 	y 	x XOR y
0	0	0
0	1	1
1	0	1
1	1	0

How many 6-input binary truth tables, τ, satisfy the formula
τ(a, b, c, d, e, f) AND τ(b, c, d, e, f, a XOR (b AND c)) = 0

for all 6-bit inputs (a, b, c, d, e, f)?

> module Main where

The mapping makes a permutation of 0..63.  We can find the
cycles using Haskell.  Then we imagin a circle with n nodes
and try to color the nodes red and green so that no two
red nodes are adjacent.  It turns out this is the fibonacci
recurrence with different initial conditions.  

> import Prelude 
> import qualified System
> import qualified Data.List as List
> import EulerLib(xor)
> import qualified Data.Bits as Bits

Implement the transformation.

> next :: Int -> Int
> next n = if n >= 64 then error "Impossible" else
>   let a = Bits.testBit n 5
>       b = Bits.testBit n 4
>       c = Bits.testBit n 3 
>       f1 = Bits.clearBit (Bits.shiftL n 1) 6 in
>   if a `xor` (b && c) then Bits.setBit f1 0 else f1

Find the cycles.

> chain :: Int -> [Int] 
> chain = iterate next

> cycles :: [[Int]]
> cycles = List.nub $ List.sort $ map (\n -> List.sort $ List.nub $ take 100 (chain n)) [0 .. 63]

> cycleLengths :: [Int]
> cycleLengths = map length cycles

Now we can find the colorings.

> colors :: [Integer]
> colors = 1 : 3 : zipWith (+) colors (tail colors)

> tables :: Integer
> tables = product (map (\n -> colors !! (n-1)) cycleLengths)

> main :: IO ()
> main = 
>   do args <- System.getArgs 
>      case args of
>       [] -> putStr ("Tables: " ++ show tables)
>       _ -> print "Usage: Euler <maxDigits>" 

$ time Euler
Tables: 15964587728784
real	0m0.019s
user	0m0.013s
sys	0m0.004s
