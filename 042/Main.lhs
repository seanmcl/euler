
The n^(th) term of the sequence of triangle numbers is given by, t_(n)
= ½n(n+1); so the first ten triangle numbers are:

1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...

By converting each letter in a word to a number corresponding to its
alphabetical position and adding these values we form a word
value. For example, the word value for SKY is 19 + 11 + 25 = 55 =
t_(10). If the word value is a triangle number then we shall call the
word a triangle word.

Using words.txt (right click and 'Save Link/Target As...'), a 16K text
file containing nearly two-thousand common English words, how many are
triangle words?

> module Main where

> import Prelude 
> import qualified Char
> import qualified System
> import qualified IO
> import qualified List
> import qualified EulerLib as Lib

> tri :: [Integer]
> tri = [(n * (n + 1)) `div` 2 | n <- [1 ..]]

> triP :: Integer -> Bool
> triP n = el n tri
>   where el _n [] = False
>         el n (h:t) = if n > h then el n t
>                      else if n == h then True
>                      else False

> charVal :: Char -> Int
> charVal c = Char.ord c - 64

> wordVal :: String -> Int
> wordVal = sum . map charVal

> solution :: [String] -> (Int, [String])
> solution words = (length l, l)
>   where l = filter (triP . toInteger . wordVal) words

> main :: IO ()
> main = 
>   do args <- System.getArgs 
>      case args of
>       [] -> print "Usage: Euler <file>" 
>       file : _ -> 
>        do s <- IO.readFile file
>           let words = Lib.lexCommaList s
>           putStr ("Solution: " ++ show (solution words))

$ time Euler words.txt
Solution: (162,["A","ABILITY","ABOVE","ACCOMPANY","ACHIEVEMENT","AGENCY","AGREE","AIR","ALREADY","AN","ANCIENT","APPARENT","APPOINT","APPROACH","ASSUME","AT","ATMOSPHERE","BAG","BAND","BANK","BAR","BEAT","BELONG","BENEATH","BONE","BOTH","BRIDGE","BUILDING","BURN","CALL","CAPACITY","CAREFUL","CASE","CHILD","CIVIL","CLOSELY","COME","CONFIDENCE","CONFIRM","CONSERVATIVE","CONSTRUCTION","CONTENT","COULD","CURRENTLY","DECISION","DEFINITION","DEMOCRATIC","DEPUTY","DESPITE","DISTINCTION","EAST","EDGE","EDUCATIONAL","EFFECT","EQUIPMENT","EVENT","FACE","FAIL","FAMILY","FEEL","FIELD","FIGURE","FLOOR","FREEDOM","FUND","FUTURE","GENTLEMAN","GREY","GROWTH","HAIR","HAPPY","HAVE","HERE","HIS","IF","INCIDENT","INCREASED","INCREASINGLY","INDIVIDUAL","INSTRUMENT","INTEND","INTENTION","IS","LAW","LEADER","LEAVE","LENGTH","LESS","LITTLE","LOVELY","MAN","MATCH","MERELY","MILK","MISTAKE","MOVE","MUCH","NEED","NOTICE","OBJECT","OBJECTIVE","OF","OIL","ONLY","OTHER","OURSELVES","PART","PASS","PATH","PERFORM","PRISON","PRIVATE","PROBABLY","PROCEDURE","QUALITY","QUESTION","RANGE","READ","REAL","RELIEF","REMOVE","REPRESENT","REQUEST","RESPOND","RIDE","SAMPLE","SAY","SEAT","SECURITY","SINGLE","SKY","SOIL","SOLICITOR","SONG","SOUTHERN","SPIRIT","START","SUGGESTION","TALL","TAX","THEORY","THREATEN","THROUGHOUT","TITLE","TOOTH","TOTALLY","TRAVEL","TYPE","UNABLE","UNDERSTAND","UPON","USE","VARIOUS","VARY","VIDEO","WAGE","WARM","WATCH","WE","WHILST","WIDELY","WOMAN"])
real	0m0.032s
user	0m0.012s
sys	0m0.004s

