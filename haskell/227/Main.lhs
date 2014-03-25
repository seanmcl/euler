
"The Chase" is a game played with two dice and an even number of
players.

The players sit around a table; the game begins with two opposite
players having one die each. On each turn, the two players with a die
roll it.  If a player rolls a 1, he passes the die to his neighbour on
the left; if he rolls a 6, he passes the die to his neighbour on the
right; otherwise, he keeps the die for the next turn.  The game ends
when one player has both dice after they have been rolled and passed,
that player has then lost.

In a game with 100 players, what is the expected number of turns the
game lasts?

Give your answer rounded to ten significant digits.

> module Main where

> import Prelude 
> import qualified Data.List as List
> import qualified System
> import qualified IO
> import qualified EulerLib as Lib
> import Ratio ((%))

Since we're dealing with expectations, we need to clearly define the
random variable.  An element of the sample space is the list of pairs
of rolls where the last roll puts the dice at the same position.  Let
X be the random variable that assigns the length of the game to the
element of the space.  Define E_n[X] be the expectation of X assuming
that the dice begin exactly n slots away from each other.  The problem
asks for the value of E_{n/2}[X].  We have a system of equations

E_0[x] = 0
E_1[x] = 8/36 + 19/36 (E_1[x] + 1) + 8/36 (E_2[X] + 1) + 1/36 (E_3[X] + 1)
...
E_n[X] = some linear combination of E_0 .. E_n

We could try to calculate the coefficients by hand, but let's just use
Haskell.  We'll use exact rational numbers.

> type Roll = (Integer, Integer)

All possible rolls.

> rolls :: [Roll]
> rolls = Lib.allPairs [1 .. 6] [1 .. 6]

A state of the game is the positions of the dice, along with the
number of players.

> data State = S { players :: Integer
>                , p1 :: Integer
>                , p2 :: Integer
>                } deriving (Read, Show)

A roll makes a transition of the state.

> step :: State -> Roll -> State
> step s (i, j) = step1 (step2 s j) i
>   where step1 s 1 = s { p1 = p1 s + 1}
>         step1 s 6 = s { p1 = p1 s - 1}
>         step1 s _ = s 
>         step2 s 1 = s { p2 = p2 s + 1}
>         step2 s 6 = s { p2 = p2 s - 1}
>         step2 s _ = s

The new distance between players is calculated with modular
arithmetic in case the game "wraps around" the circle.

> distance :: State -> Integer
> distance (S players p1 p2) = 
>   min (Lib.posmod (p1 - p2) players) (Lib.posmod (p2 - p1) players)

Get all 36 steps from a given state.

> stepAll :: State -> [Integer]
> stepAll s = 
>   let ints = List.group $ List.sort $ map (distance . step s) rolls 
>       mapFn i = case List.find (\l -> head l == i) ints of
>                   Nothing -> 0 
>                   Just l -> fromIntegral (length l) in
>   map mapFn [0 .. players s `div` 2]

Now we can find the coefficients (times 36) of the equations.

> recEqs :: Integer -> [[Integer]]
> recEqs n = 
>   let states = map (\i -> S { players = n, p1 = 0, p2 = i })
>                    [1 .. n `div` 2]
>       eqs = map stepAll states in
>   map (\(h:t) -> (sum t + h) : t) eqs

To solve this system, we use Gaussian elimination.  gauss1 takes
a set of equations, numbered with the row position, and eliminates
the first variable.  

> gauss1 :: [(Int, [Rational])] -> [(Int, [Rational])] 
> gauss1 ((i, ns) : rest) = 
>   let coef = ns !! i
>       mul = recip (1 - coef)
>       ns' = map (\(j, x) -> if i == j then 0
>                  else mul * x) (Lib.number ns)
>       mapFn (j, l) =
>         let coef1 = l !! i
>             ns'' = map (* coef1) ns'
>             l' = Lib.setElem l i 0 in
>         (j, zipWith (+) l' ns'') in
>   (map mapFn rest)
> gauss1 _ = error "Impossible" 

The final variable will be the value we seek, E_{n/2}.  

> gauss :: [[Integer]] -> Rational
> gauss eqs = 
>   let reqs = map (map (\k -> k % 36)) eqs ++ [replicate (length eqs) 0 ++ [1]]
>       reqs1 = zip [1 ..] reqs
>       elim = iterate gauss1 reqs1 in
>   case List.find (\l -> length l == 1) elim of
>     Just [(_, h : _)] -> h
>     _ -> error "Impossible" 

We can then convert the expectation to double precision.

> expect :: Integer -> Double
> expect n = fromRational $ gauss $ recEqs n

> main :: IO ()
> main = 
>   do args <- System.getArgs 
>      case args of
>       [] -> print "Usage: Euler <int>" 
>       n : _ -> putStr ("Expectation for " ++ n ++ " players: " ++ (show $ expect $ read n))

$ time Euler 100
Expectation for 100 players: 3780.61862178479
real	0m0.163s
user	0m0.144s
sys	0m0.014s
