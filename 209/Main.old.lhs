
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

> import Prelude 
> import qualified System
> import qualified Data.Map as Map
> import qualified Data.List as List
> import Data.List ( (\\) )
> import Data.Map (Map)
> import qualified EulerLib as Lib
> import EulerLib(xor)
> import qualified Data.Bits as Bits

A row of a truth table

> forced :: Int -> (Int, Int)
> forced n = 
>   let a = Bits.testBit n 5
>       b = Bits.testBit n 4
>       c = Bits.testBit n 3 
>       f1 = Bits.shiftL n 1
>       f1' = if a `xor` (b && c) then Bits.setBit f1 0 else f1
>       f2 = Bits.shiftR n 1
>       f2' = if not(a && b) then Bits.setBit f2 5 else f2 in
>   (f1', f2')

A truth table.

> type Table = Map Int Bool

> nextEntry :: Table -> Maybe Int
> nextEntry m = 
>   case List.sort $ ([0 .. 63] \\ Map.keys m) of
>     [] -> Nothing 
>     h:_ -> Just h

Set an element of the table

> set :: Table -> Int -> Bool -> Maybe Table 

You can always set a row to False 

> set m k False = 
>   case Map.lookup k m of 
>     Just False -> Just m
>     Just True -> Nothing 
>     Nothing -> Just $ Map.insert k False m

For True, you need to check the forced rows

> set m k True =
>   case Map.lookup k m of
>     Just True -> Just m
>     Just False -> Nothing
>     Nothing -> 
>       let (k1, k2) = forced k 
>           m1 = Map.insert k True m in
>       case set m1 k1 False of
>         Nothing -> Nothing 
>         Just m2 -> set m2 k2 False

Split a table

> split :: Table -> Maybe [Table]
> split m = 
>   case nextEntry m of
>     Nothing -> Nothing -- Completed 
>     Just k -> 
>       case (set m k False, set m k True) of 
>         (Nothing, Nothing) -> Just [] -- Inconsistent
>         (Nothing, Just m) -> Just [m] 
>         (Just m, Nothing) -> Just [m]
>         (Just m1, Just m2) -> Just [m1, m2]

> tables :: IO Integer
> tables = table [Map.empty] 0
>   where table [] n = return n
>         table (h:t) n = 
>           case split h of
>             Nothing -> do if n `mod` 10000 == 0 then print ("found: " ++ show n ++ " tabs: " ++ show (length t)) else return ()
>                           table t (n + 1)
>             Just [] -> table t n
>             Just t' -> table (t' ++ t) n

> main :: IO ()
> main = 
>   do args <- System.getArgs 
>      ts <- tables
>      case args of
>       [] -> putStr ("Tables: " ++ show ts)
>       _ -> print "Usage: Euler <maxDigits>" 

$ time Euler 100
Nonbouncy with at most 100 digits: 51161058134250
real	0m0.037s
user	0m0.030s
sys	0m0.005s
