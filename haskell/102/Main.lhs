
Three distinct points are plotted at random on a Cartesian plane, for
which -1000 <= x, y <= 1000, such that a triangle is formed.

Consider the following two triangles:

A(-340,495), B(-153,-910), C(835,-947)

X(-175,41), Y(-421,-714), Z(574,-645)

It can be verified that triangle ABC contains the origin, whereas
triangle XYZ does not.

Using triangles.txt (right click and 'Save Link/Target As...'), a 27K
text file containing the co-ordinates of one thousand "random"
triangles, find the number of triangles for which the interior
contains the origin.

NOTE: The first two examples in the file represent the triangles in
the example given above.

> module Main where

> import Prelude 
> import qualified System
> import qualified IO

> type Point = (Int, Int)

> origin :: Point
> origin = (0, 0)

> type Triangle = (Point, Point, Point)

> diff :: Point -> Point -> Point 
> diff (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

> cross :: Point -> Point -> Int
> cross (x1, y1) (x2, y2) = x1 * y2 - y1 * x2

lineSideTest p1 p2 p3 
returns True iff p3 is to the right of ray (p1, p2).

> lineSideTest :: Point -> Point -> Point -> Bool
> lineSideTest p1 p2 p3 =
>   let p2' = diff p2 p1
>       p3' = diff p3 p1 in
>   cross p2' p3' > 0

A triangle contains the origin if, when traversing the triangle in a
cycle, the origin is either to the right of all its rays, or to the
left of all its rays.

> containsOrigin :: Triangle -> Bool
> containsOrigin (p1, p2, p3) = 
>   let t1 = lineSideTest p1 p2 origin
>       t2 = lineSideTest p2 p3 origin
>       t3 = lineSideTest p3 p1 origin in
>   (t1 && t2 && t3) || (not t1 && not t2 && not t3)

> nlex :: String -> [Int]
> nlex s = case lex s of
>            [("","")] -> []
>            [(",",rest)] -> nlex rest
>            [("-",rest)] -> case nlex rest of
>                              [] -> error "Parse error" 
>                              h:t -> -h:t
>            [(n, rest)] -> read n : nlex rest
>            _ -> error "Impossible" 

> parse :: String -> [Triangle]
> parse s = 
>   let ls = lines s
>       pts = map nlex ls
>       ts = map listToTri pts in
>   ts
>     where listToTri [x1,x2,x3,x4,x5,x6] = ((x1, x2), (x3, x4), (x5, x6))
>           listToTri _ = error "Impossible" 

> main :: IO ()
> main = do 
>   args <- System.getArgs 
>   case args of
>     [] -> print "Usage: Euler <file>" 
>     file : _ -> do 
>       s <- IO.readFile file
>       let ts = parse s
>           ts' = filter containsOrigin ts
>       putStr ("There are " ++ show (length ts') ++ " triangles containing the origin")

$ time Euler triangles.txt
There are 228 triangles containing the origin
real	0m0.074s
user	0m0.068s
sys	0m0.005s
