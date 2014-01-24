
Starting in the top left corner of a 2×2 grid, there are 6 routes
(without backtracking) to the bottom right corner.

How many routes are there through a 20×20 grid?

> module Main where

> import Prelude 
> import qualified Control.Monad.State.Lazy as State
> import qualified Data.Map as Map

Let (n,k) be the starting point, and (0, 0) be the finish.
We assume you must never go up or to the left.  
Let P(n, k) be the number of paths from (n, k) to (0,0)
Then P(n, k) = P(n-1, k) + P(n, k-1).  We should solve
this with dynamic programming, which in this case simply
means memoizing the lookups.  This is because the smaller
values will be computed many times.

Note that the recurrence looks familiar.  It is Pascal's triangle.
So the answer for (n, n) is Binomial[2n, n].

In[9]:= Binomial[40,20]
Out[9]= 137846528820

But let's compute it from scratch

> type Store = Map.Map (Integer, Integer) Integer

> p :: Integer -> Integer -> State.State Store Integer
> p 0 _ = return 1
> p _ 0 = return 1
> p n k = do store <- State.get
>            case Map.lookup (n, k) store of
>              Just pnk -> return pnk
>              Nothing -> 
>                do p1 <- p (n-1) k
>                   p2 <- p n (k-1)
>                   let pnk = p1 + p2
>                   store <- State.get
>                   State.put (Map.insert (n, k) pnk store)
>                   return pnk

> grid :: Integer -> Integer -> Integer
> grid n k = State.evalState (p n k) Map.empty

> main :: IO ()
> main = print ("Number of paths is: " ++ show (grid 20 20))

~/save/versioned/projects/compiler/euler/15
$ time Euler 
"Number of paths is: 137846528820"

real	0m0.009s
user	0m0.004s
sys	0m0.004s
