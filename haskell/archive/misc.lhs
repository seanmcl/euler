
divide n k return a list of the digits of long division of n by k

> divide :: Int -> Int -> [Int]
> divide n k = 
>   if k == 0 then [] else
>   let z = zeros n k 
>       pre = replicate (z-1) 0
>       k' = k * 10 ^ z
>       d = k' `div` n 
>       k'' = k' `mod` n in 
>   pre ++ [d] ++ divide n k''

> zeros :: Int -> Int -> Int
> zeros n k = if n <= k then 0 else 1 + zeros n (10 * k)



> foldri :: (Int -> a -> b -> b) -> b -> [a] -> b
> foldri f b l = foldri' f b l 0
>   where foldri' _f b [] _ = b
>         foldri' f b (h:t) i = f i h (foldri' f b t (i + 1))

> listToStore :: [[Int]] -> Store 
> listToStore ls = foldri (\i l m -> foldri (\j k m' -> Map.insert (i, j) k m') m l) Map.empty ls
