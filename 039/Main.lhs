
If p is the perimeter of a right angle triangle with integral length
sides, {a,b,c}, there are exactly three solutions for p = 120.

{20,48,52}, {24,45,51}, {30,40,50}

For which value of p ≤ 1000, is the number of solutions maximised?

> module Main where

> import Prelude 
> import qualified Data.List as List
> import qualified Control.Monad as M

Note that if x + y + z = n and x^2 + y^2 = z^2 then
x^2 + y^2 = (n - x - y)^2 

In[3]:= Expand[(n-x-y)^2]

         2            2                    2
Out[3]= n  - 2 n x + x  - 2 n y + 2 x y + y

So if we fix y then 

x = (n^2 - 2 n y) / (2 (n - y)) 

when that is an integer.  

solns n

gives the triples that sum to n

> solns :: Int -> [(Int, Int, Int)]
> solns n = 
>   do 
>     y <- [1 .. n-1]
>     let xn = n^2 - 2 * n * y
>         xd = 2 * (n - y)
>         x = xn `div` xd
>         z = n - x - y
>     M.guard (xn `mod` xd == 0)
>     M.guard (0 < x && x < y)
>     M.guard (y < z)
>     M.guard (x^2 + y^2 == z^2)
>     return (x, y, z)

> solution :: (Int, Int)
> solution = maximum (map (\n -> (length $ solns n, n)) [1 .. 1000])

> main :: IO ()
> main = putStr ("Solution: " ++ show solution)

$ time Euler
Solution: (8,840)
real	0m0.246s
user	0m0.239s
sys	0m0.005s

