
Working from left-to-right if no digit is exceeded by the digit to its
left it is called an increasing number; for example, 134468.

Similarly if no digit is exceeded by the digit to its right it is
called a decreasing number; for example, 66420.

We shall call a positive integer that is neither increasing nor
decreasing a "bouncy" number; for example, 155349.

As n increases, the proportion of bouncy numbers below n increases
such that there are only 12951 numbers below one-million that are not
bouncy and only 277032 non-bouncy numbers below 10^(10).

How many numbers below a googol (10^(100)) are not bouncy?

> module Main where

Looks like dynamic programming.

Let inc(n, k) be the number of increasing sequences of length n, 
starting with k.  Note that '0' never occurs in increasing numbers.

inc(1, k) = 1
inc(n, 9) = inc(n-1, 9)
inc(n, 8) = inc(n-1, 8) + inc(n-1, 9)
...
inc(n, 1) = inc(n-1, 1) + ... + inc(n-1, 9)

Let dec(n, k) be the number of decreasing sequences of length n, 
starting with k.  Note that 0 can occur in a decreasing number,
but only as a chain at the end of the number.

dec(1, k) = 1
dec(n, 1) = dec(n-1, 1) + 1 -- for adding a string of n-1 zeros
dec(n, 2) = dec(n-1, 2) + dec(n-1, 1) + 1
...
dec(n, 9) = dec(n-1, 9) + ... + dec(n-1, 1) + 1

If we sum inc(i, j) + dec(i, j), 0 < i < 100, 0 <= j <= 9
we will nearly have the answer.  The problem is that we have
double counted things like "555".  So we have to subtract
9 * 99.  Finally, we need to add 1 for "0".

Also good to know, from another solution:

http://mathworld.wolfram.com/LatticePath.html
I should have guessed from the recurrance equations that 
the binomial coefficients would somehow be involved.

> import Prelude 
> import qualified System
> import qualified Data.Map as Map
> import qualified Data.List as List
> import Data.Map (Map)
> import qualified Control.Monad.State as State
> import Control.Monad.State(State)

> type DMap = Map (Integer, Integer) Integer

Inefficient sanity check for small numbers 

> nonbouncy :: Integer -> Integer
> nonbouncy n = 
>   let f n = 
>         let sn = show n
>             sn' = List.sort sn in
>         sn == sn' || sn == reverse sn'
>   in toInteger $ length $ List.filter f [1 .. n]

Dynamic programming

> inc :: Integer -> Integer -> State DMap Integer
> inc 1 _k = return 1
> inc n k = do m <- State.get
>              case Map.lookup (n, k) m of
>                Just t -> return t
>                Nothing -> 
>                  do incs <- mapM (inc (n-1)) [k .. 9]
>                     let t = sum incs
>                     m <- State.get
>                     State.put (Map.insert (n, k) t m)
>                     return t

> increasing :: Integer -> Integer
> increasing digits = State.evalState incr Map.empty
>   where incr = do incs <- mapM (uncurry inc) [(n, k) | n <- [1 .. digits], k <- [1 .. 9]]
>                   return (sum incs)

> dec :: Integer -> Integer -> State DMap Integer
> dec 1 _k = return 1
> dec n k = do m <- State.get
>              case Map.lookup (n, k) m of
>                Just t -> return t
>                Nothing -> 
>                  do decs <- mapM (dec (n-1)) [1 .. k]
>                     let t = 1 + sum decs
>                     m <- State.get
>                     State.put (Map.insert (n, k) t m)
>                     return t

> decreasing :: Integer -> Integer
> decreasing digits = State.evalState decr Map.empty
>   where decr = do decs <- mapM (uncurry dec) [(n, k) | n <- [1 .. digits], k <- [1 .. 9]]
>                   return (sum decs)

> doit :: Integer -> Integer
> doit n = increasing n + decreasing n - 9 * n -- + 1

> main :: IO ()
> main = 
>   do args <- System.getArgs 
>      case args of
>       [n] -> putStr ("Nonbouncy with at most " ++ n ++ " digits: " ++ show (doit (read n)))
>       _ -> print "Usage: Euler <maxDigits>" 

$ time Euler 100
Nonbouncy with at most 100 digits: 51161058134250
real	0m0.037s
user	0m0.030s
sys	0m0.005s
