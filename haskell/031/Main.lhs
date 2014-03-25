
In England the currency is made up of pound, £, and pence, p, and
there are eight coins in general circulation:

    1p, 2p, 5p, 10p, 20p, 50p, £1 (100p) and £2 (200p).

It is possible to make £2 in the following way:

    1×£1 + 1×50p + 2×20p + 1×5p + 1×2p + 3×1p

How many different ways can £2 be made using any number of coins?

> module Main where

> import Prelude 
> import qualified System
> import qualified IO
> import qualified Maybe

> coins :: [Int]
> coins = [1, 2, 5, 10, 20, 50, 100, 200]

> extend :: (Int, [Int]) -> [(Int, [Int])]
> extend (0, acc) = [(0, acc)]
> extend (n, []) = concat $ Maybe.mapMaybe (\c -> if c <= n then Just $ extend (n - c, [c]) else Nothing) coins
> extend (n, acc@(h:_)) = concat $ Maybe.mapMaybe (\c -> if c <= n && c <= h then Just $ extend (n - c, c:acc) else Nothing) coins

> change :: Int -> [[Int]]
> change n = map snd $ extend (n, [])

> main :: IO ()
> main = 
>   do args <- System.getArgs 
>      case args of
>       [] -> print "Usage: Euler <pence:int>" 
>       h : _ -> putStr ("There are " ++ show (length $ change $ read h)
>                        ++ " ways to make change for " ++ h ++ " pence.")


