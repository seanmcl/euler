
{-
The file, poker.txt, contains one-thousand random hands dealt to two
players. Each line of the file contains ten cards (separated by a single space):
the first five are Player 1's cards and the last five are Player 2's cards. You
can assume that all hands are valid (no invalid characters or repeated cards),
each player's hand is in no specific order, and in each hand there is a clear
winner.

How many hands does Player 1 win?
-}

module Main where

import Prelude
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Data.Map (Map)
import Control.Monad (foldM)
import qualified IO

-- -----------------------------------------------------------------------------

findM :: (a -> b -> Maybe c) -> Map a b -> Maybe c
findM f m = Map.foldrWithKey ffn Nothing m
  where ffn k x acc =
          if Maybe.isJust acc then acc
          else f k x

-- -----------------------------------------------------------------------------

data Suit = Club | Diamond | Heart | Spade
  deriving (Eq, Ord)

instance Show Suit where
  show c = case c of
     Club    -> "C"
     Diamond -> "D"
     Heart   -> "H"
     Spade   -> "S"

charToSuit :: Char -> Suit
charToSuit c = case c of
  'C' -> Club
  'D' -> Diamond
  'H' -> Heart
  'S' -> Spade
  _ -> error ("Can't read suit: " ++ [c])

-- -----------------------------------------------------------------------------

data CNum = N2 | N3 | N4 | N5 | N6 | N7 | N8 | N9 | N10 | Jack | Queen | King | Ace
  deriving (Eq, Ord)

instance Show CNum where
  show c = case c of
    N2    -> "2"
    N3    -> "3"
    N4    -> "4"
    N5    -> "5"
    N6    -> "6"
    N7    -> "7"
    N8    -> "8"
    N9    -> "9"
    N10   -> "T"
    Jack  -> "J"
    Queen -> "Q"
    King  -> "K"
    Ace   -> "A"

cnumToInt :: CNum -> Int
cnumToInt n = case n of
  N2    -> 2
  N3    -> 3
  N4    -> 4
  N5    -> 5
  N6    -> 6
  N7    -> 7
  N8    -> 8
  N9    -> 9
  N10   -> 10
  Jack  -> 11
  Queen -> 12
  King  -> 13
  Ace   -> 14

charToCnum :: Char -> CNum
charToCnum n = case n of
  '2' -> N2
  '3' -> N3
  '4' -> N4
  '5' -> N5
  '6' -> N6
  '7' -> N7
  '8' -> N8
  '9' -> N9
  'T' -> N10
  'J' -> Jack
  'Q' -> Queen
  'K' -> King
  'A' -> Ace
  _ -> error ("Can't read num: " ++ [n])

-- -----------------------------------------------------------------------------

data Card = Card { num :: CNum, suit :: Suit }

instance Show Card where
  show c = show (num c) ++ show (suit c)

instance Eq Card where
  (==) l r = num l == num r

instance Ord Card where
  compare l r = compare (num l) (num r)

readCard :: String -> Card
readCard s = case s of
 [n, s] -> Card { num = charToCnum n, suit = charToSuit s }
 _ -> error ("Can't read card: " ++ s)

-- -----------------------------------------------------------------------------

data Hand = Hand__ {cards :: [Card]}
  deriving (Eq, Ord)

hand :: [Card] -> Hand
hand l = Hand__ (reverse (List.sort l))

instance Show Hand where
  show l = show (cards l)

-- -----------------------------------------------------------------------------

data Game = Game { p1 :: Hand, p2 :: Hand }

instance Show Game where
  show g = "p1: " ++ show (p1 g) ++ " p2: " ++ show (p2 g)

game :: String -> Game
game s =
  let ws = words s
      cs = map readCard ws
      p1 = hand (take 5 cs)
      p2 = hand (drop 5 cs)
  in Game {p1 = p1, p2 = p2}

-- -----------------------------------------------------------------------------

data Kind = None
          | Pair CNum
          | TwoPair (CNum, CNum) -- hi, lo
          | Three CNum
          | Straight CNum
          | Flush
          | FullHouse (CNum, CNum) -- 3, 2
          | Four CNum
          | StraightFlush CNum
  deriving (Eq, Ord, Show)

-- -----------------------------------------------------------------------------

-- invariant: the remaining cards are sorted, greatest first
data Score = Score (Kind, [CNum])
  deriving (Eq, Show)

instance Ord Score where
  compare (Score (k1, r1)) (Score (k2, r2)) =
    case compare k1 k2 of
      EQ -> compare r1 r2
      o -> o

type HMap = Map CNum [CNum]

handToHMap :: Hand -> HMap
handToHMap h = aux Map.empty (cards h)
  where aux m []     = m
        aux m (c:cs) = Map.insertWith (++) (num c) [num c] (aux m cs)

classCards :: HMap -> [CNum]
classCards m =
  reverse $ List.sort $ Map.foldrWithKey (\c l acc -> l ++ acc) [] m

straight :: Hand -> Maybe CNum
straight h =
  let cs = cards h
      ns = map (cnumToInt . num) cs
      aux k [] = True
      aux k (c:cs) = k == c+1 && aux c cs in
  if aux (head ns) (tail ns) then Just (num (head cs)) else Nothing

flush :: Hand -> Bool
flush h =
  let cs = cards h
      s = suit (head (cards h)) in
  all (\c -> suit c == s) cs

classify :: Hand -> Score
classify h =
  let m = handToHMap h
      ffn k n cs = if length cs == k then Just n else Nothing in
  case findM (ffn 4) m of
    Just n ->
        let m' = Map.delete n m in
        Score (Four n, classCards m')
    Nothing -> case findM (ffn 3) m of
      Just n ->
        let m' = Map.delete n m in
        case findM (ffn 2) m' of
          Just k -> Score (FullHouse (n, k), [])
          Nothing -> Score (Three n, classCards m')
      Nothing -> case findM (ffn 2) m of
        Just n ->
          let m' = Map.delete n m in
          case findM (ffn 2) m' of
            Nothing -> Score (Pair n, classCards m')
            Just k ->
              let m'' = Map.delete k m' in
              let [k', n'] = List.sort [k, n] in
              Score (TwoPair (n', k'), classCards m'')
        Nothing -> case (straight h, flush h) of
          (Just n, True) -> Score (StraightFlush n, [])
          (Just n, False) -> Score (Straight n, [])
          (Nothing, True) -> Score (Flush, classCards m)
          (Nothing, False) -> Score (None, classCards m)

play :: Game -> IO Bool
play g = do
  putStrLn (show g)
  let s1 = classify (p1 g)
      s2 = classify (p2 g)
      b = s1 > s2
  putStrLn $ "Player 1: " ++ show s1
  putStrLn $ "Player 2: " ++ show s2
  if b then putStrLn "Player 1 wins" else putStrLn "Player 2 wins"
  return b

-- -----------------------------------------------------------------------------

main :: IO ()
main = do
  s <- IO.readFile "poker.txt"
  let gs = map game (lines s)
      ffn n g = do
        b <- play g
        if b then return (n+1) else return n
  n <- foldM ffn 0 gs
  putStr ("Hands won by player 1: " ++ show n ++ "\n")

{-
$ time ./Euler
Curious: Just 142857
real	0m0.665s
user	0m0.650s
sys	0m0.008s
-}
