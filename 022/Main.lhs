
Using names.txt (right click and 'Save Link/Target As...'), a 46K text
file containing over five-thousand first names, begin by sorting it
into alphabetical order. Then working out the alphabetical value for
each name, multiply this value by its alphabetical position in the
list to obtain a name score.

For example, when the list is sorted into alphabetical order, COLIN,
which is worth 3 + 15 + 12 + 9 + 14 = 53, is the 938th name in the
list. So, COLIN would obtain a score of 938 × 53 = 49714.

What is the total of all the name scores in the file?

> module Main where

> import Prelude 
> import qualified Data.List as List
> import qualified Data.Char as Char
> import qualified Data.Map as Map
> import Data.Map (Map)
> import qualified System
> import qualified IO

To get the numeric value, we could use the ascii code and Char.ord,
but this is not portable.  Instead use a map.

> alpha :: String
> alpha = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

> alphaMap :: Map Char Int
> alphaMap = fst $ foldl (\(m, i) c -> (Map.insert c i m, i+1)) 
>                        (Map.empty, 1) alpha

> nameVal :: String -> Int
> nameVal = sum . map charVal
>   where charVal c = case Map.lookup c alphaMap of
>                       Nothing -> error "Impossible" 
>                       Just n -> n

> score :: [String] -> Int
> score s = sum $ map (\(i, n) -> i * nameVal n) (zip [1 .. length s] s)

> nlex :: String -> [String]
> nlex s = case lex s of
>            [("","")] -> []
>            [(",",rest)] -> nlex rest
>            [(n, rest)] -> unquote n : nlex rest
>            _ -> error "Impossible" 
>   where unquote ('\"' : s) = take (length s - 1) s
>         unquote _ = error "Impossible" 

> main :: IO ()
> main = 
>   do args <- System.getArgs 
>      case args of
>       [] -> print "Usage: Euler <file>" 
>       file : _ -> 
>        do s <- IO.readFile file
>           let names = List.sort $ nlex s
>           putStr ("The names in file " ++ file ++ " score " ++ show (score names))

$ time Euler names.txt
The names in file names.txt score 871198282
real	0m0.059s
user	0m0.050s
sys	0m0.007s


