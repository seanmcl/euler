
A positive fraction whose numerator is less than its denominator is
called a proper fraction.  For any denominator, d, there will be d−1
proper fractions; for example, with d = 12: ^(1)/_(12) , ^(2)/_(12) ,
^(3)/_(12) , ^(4)/_(12) , ^(5)/_(12) , ^(6)/_(12) , ^(7)/_(12) ,
^(8)/_(12) , ^(9)/_(12) , ^(10)/_(12) , ^(11)/_(12) .

We shall call a fraction that cannot be cancelled down a resilient
fraction.  Furthermore we shall define the resilience of a
denominator, R(d), to be the ratio of its proper fractions that are
resilient; for example, R(12) = ^(4)/_(11) .  In fact, d = 12 is the
smallest denominator having a resilience R(d) < ^(4)/_(10) .

Find the smallest denominator d, having a resilience R(d) <
^(15499)/_(94744) .

> module Main where
 
> import Prelude 
> import qualified System
> import qualified EulerLib as Lib
> import qualified Ratio 
> import qualified Data.List as List
> import qualified Data.Map as Map
> import Ratio ((%))
> import System.IO.Unsafe (unsafePerformIO)

> sieve :: [Integer] -> [Integer] -> [Integer]
> sieve [] l = l
> sieve (h:t) l = sieve t (List.filter (\x -> x `mod` h /= 0) l)

> resilient :: Integer -> Integer
> resilient n = 
>   let ds = Map.keys $ Lib.primeDivisors n in
>   toInteger $ length $ sieve ds [1 .. n-1]

> totient1 :: Integer -> Integer
> totient1 n = 
>   let f p k s = (p-1) * p^(k-1) * s in
>   Map.foldWithKey f 1 (Lib.primeDivisors n)

> totient2 :: Integer -> Integer
> totient2 n = 
>   let f p s = (1 - 1 % p) * s in
>   Ratio.numerator (n % 1 * foldr f 1 (Map.keys $ Lib.primeDivisors n))

> totient :: Integer -> Integer
> totient = totient1

> rat :: Integer -> Double
> rat n = fromRational (totient n % (n-1))

> resLt :: Rational -> Integer -> Bool
> resLt r n = (totient n % (n - 1)) < r

> findit :: Rational -> Maybe Integer
> findit r = 
>   let f n = unsafePerformIO
>               (do if n `mod` 1 == 0 
>                    then print (show n)
>                    else return ()
>                   return (resLt r n))
>   in List.find f [223092870 .. 6469693230]


> main :: IO ()
> main = 
>   do args <- System.getArgs 
>      case args of
>       ["rat", n]  -> putStr ("Rat: " ++ show (rat (read n)))
>       ["divs", n]  -> putStr ("Divs: " ++ show (Lib.primeDivisors (read n)))
>       [n, k]  -> putStr ("Min: " ++ show (findit (read n % read k)))
>       _ -> print "Usage: Euler <num> <denom>" 
