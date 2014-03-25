The series, 1^(1) + 2^(2) + 3^(3) + ... + 10^(10) = 10405071317.

Find the last ten digits of the series, 
1^(1) + 2^(2) + 3^(3) + ... + 1000^(1000).

> module Main where

> import Prelude 
> import qualified System
> import qualified IO

> power :: Integer -> Integer -> Integer
> power _n 0 = 1
> power n k | k `mod` 2 == 0 = let x = power n (k `div` 2) in
>                                      x * x
>           | otherwise = n * power n (k - 1)

> series :: Integer -> Integer
> series n = sum (map (\i -> power i i) [1 .. n])

> last10 :: Integer -> Integer
> last10 n = n `mod` (power 10 10)

> main :: IO ()
> main = 
>   do args <- System.getArgs 
>      case args of
>       [] -> print "Usage: Euler <int>" 
>       h : _ -> putStr ("last10 " ++ show h ++ " = " ++ (show $ last10 $ series $ read h))

