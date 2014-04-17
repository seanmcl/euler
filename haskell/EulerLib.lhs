
> module EulerLib ( primes
>                 , firstFactor
>                 , composite
>                 , isPrime
>                 , primeDivisors
>                 , divisors
>                 , properDivisors
>                 , allPairs
>                 , allCombs
>                 , findRem
>                 , insert
>                 , number
>                 , setElem
>                 , posmod
>                 , xor
>                 , foldri
>                 , digits
>                 , fromDigits
>                 , log10
>                 , lexCommaList
>                 , squareRoot
>                 ) where
>

> import Prelude
> import qualified Data.List as List
> import qualified Data.Map as Map
> import Data.Map (Map)

-- > import qualified Data.Edison.Coll.SplayHeap as Bag

Misc

Lex a string of the form "x1", "x2", ... , "xn" to
the constituent strings [x1, x2, ...]

> lexCommaList :: String -> [String]
> lexCommaList s =
>   case lex s of
>     [("","")] -> []
>     [(",",rest)] -> lexCommaList rest
>     [(n, rest)] -> unquote n : lexCommaList rest
>     _ -> error "Impossible"
>    where unquote ('\"' : s) = take (length s - 1) s
>          unquote _ = error "Impossible"

> log10 :: Integer -> Integer
> log10 n | n < 10 = 0
> log10 n = 1 + log10 (n `div` 10)

> xor :: Bool -> Bool -> Bool
> xor a b = (a || b) && not (a && b)

> foldri :: (Int -> a -> b -> b) -> b -> [a] -> b
> foldri f b l = foldr (uncurry f) b (zip [0 .. ] l)

> digits :: Integer -> [Integer]
> digits n = reverse (dig n)
>   where dig 0 = []
>         dig n =
>           let m = n `mod` 10
>               d = n `div` 10 in
>           m : dig d

> fromDigits :: [Integer] -> Integer
> fromDigits l = dig (reverse l)
>   where dig [] = 0
>         dig (h:t) = h + 10 * dig t

> (^!) :: Num a => a -> Int -> a
> (^!) x n = x^n

> squareRoot :: Integer -> Integer
> squareRoot 0 = 0
> squareRoot 1 = 1
> squareRoot n =
>    let twopows = iterate (^!2) 2
>        (lowerRoot, lowerN) =
>           last $ takeWhile ((n>=) . snd) $ zip (1:twopows) twopows
>        newtonStep x = div (x + div n x) 2
>        iters = iterate newtonStep (squareRoot (div n lowerN) * lowerRoot)
>        isRoot r  =  r^!2 <= n && n < (r+1)^!2
>   in  head $ dropWhile (not . isRoot) iters

Prime number utilities

> filtlist :: [Integer] -> [Integer]
> filtlist (h : t) = h : filter (\n -> n `mod` h /= 0) (filtlist t)
> filtlist _ = error "Impossible"

> slowPrimes :: [Integer]
> slowPrimes = filtlist [2 ..]

> firstFactor :: Integer -> Maybe Integer
> firstFactor n = List.find (\x -> n `mod` x == 0)
>                 (takeWhile (\x -> x * x <= n) slowPrimes)

> composite :: Integer -> Bool
> composite n = any (\x -> n `mod` x == 0)
>               (takeWhile (\x -> x * x <= n) slowPrimes)

> isPrime :: Integer -> Bool
> isPrime n = n > 1 && not (composite n)

> primes :: [Integer]
> primes = filter isPrime [2..]

> incrMap :: Integer -> Map Integer Integer -> Map Integer Integer
> incrMap n m = case Map.lookup n m of
>                 Nothing -> Map.insert n 1 m
>                 Just k -> Map.insert n (k+1) m

> primeDivisors :: Integer -> Map Integer Integer
> primeDivisors n = primed n Map.empty
>   where primed n m =
>           case firstFactor n of
>             Nothing -> incrMap n m
>             Just k -> primed k (primed (n `div` k) m)

> primeFactors :: Integer -> [Integer]
> primeFactors n = factorsAux n [1]
>   where factorsAux n fs =
>           case firstFactor n of
>             Nothing -> n : fs
>             Just k -> factorsAux (n `div` k) (k : fs)

> divisors :: Integer -> [Integer]
> divisors = List.sort . List.nub . map product . List.subsequences . primeFactors

> properDivisors :: Integer -> [Integer]
> properDivisors n =
>   let ds = divisors n in
>   take (length ds - 1) ds

List utilities

> allPairs :: [a] -> [b] -> [(a, b)]
> allPairs [] _ = []
> allPairs _ [] = []
> allPairs (h : t) l = map (\x -> (h, x)) l ++ allPairs t l

> allCombs :: [[a]] -> [[a]]
> allCombs [] = [[]]
> allCombs (h:t) = concat $ map (\l -> map (\x -> x:l) h) (allCombs t)

> findRem :: (a -> Bool) -> [a] -> Maybe (a, [a])
> findRem _ [] = Nothing
> findRem p (h:t) =
>   if p h then Just (h, t) else
>   case findRem p t of
>     Nothing -> Nothing
>     Just (x, t') -> Just(x, h:t')

> insert :: Int -> [Int] -> [Int]
> insert x [] = [x]
> insert x (l@(h:t)) = if x <= h then x:l else h:insert x t

> number :: [a] -> [(Int, a)]
> number xs = zip [0 .. length xs - 1] xs

> setElem :: [a] -> Int -> a -> [a]
> setElem (_:t) 0 x = x:t
> setElem (h:t) n x = h : setElem t (n-1) x
> setElem _ _ _ = error "Impossible"

> posmod :: Integer -> Integer -> Integer
> posmod n k =
>   let m = n `mod` k in
>   if m >= 0 then m else
>   let d = n `div` k in
>   (n + (d + 1) * k) `mod` k

-- > divisors :: Integer -> [Integer]
-- > divisors n =
-- >   let pdivs = primeDivisors n
-- >     Nothing -> Bag.singleton n
-- >     Just k -> Bag.union (primeDivisors k) (primeDivisors (n `div` k))
