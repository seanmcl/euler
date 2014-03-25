
You are given the following information, but you may prefer to do some
research for yourself.

    * 1 Jan 1900 was a Monday.
    * Thirty days has September,
      April, June and November.
      All the rest have thirty-one,
      Saving February alone,
      Which has twenty-eight, rain or shine.
      And on leap years, twenty-nine.
    * A leap year occurs on any year evenly divisible by 4, but not on
      a century unless it is divisible by 400.

How many Sundays fell on the first of the month during the twentieth
century (1 Jan 1901 to 31 Dec 2000)?

> module Main where

> import Prelude 

> data Day = Mon | Tues | Wed | Thurs | Fri | Sat | Sun
>   deriving (Read, Show)

> weekdays :: [Day]
> weekdays = Mon : Tues : Wed : Thurs : Fri : Sat : Sun : weekdays

> leapyear :: Int -> Bool
> leapyear n = n `mod` 4 == 0 && 
>              (n `mod` 400 == 0 || not (n `mod` 100 == 0))

> yeardays1 :: Int -> [Int]
> yeardays1 n = jan ++ feb n ++ mar ++ apr ++ may ++ jun ++ jul ++ aug 
>               ++ sep ++ oct ++ nov ++ dec 
>   where jan = [1 .. 31]
>         feb n = if leapyear n then [1 .. 29] else [1 .. 28]
>         mar = [1 .. 31]
>         apr = [1 .. 30]
>         may = [1 .. 31]
>         jun = [1 .. 30]
>         jul = [1 .. 31]
>         aug = [1 .. 31]
>         sep = [1 .. 30]
>         oct = [1 .. 31]
>         nov = [1 .. 30]
>         dec = [1 .. 31]

> yeardays :: [Int]
> yeardays = concat (map yeardays1 [1900 .. 2000])

> days :: [(Day, Int)]
> days = zip weekdays yeardays

> sundays :: Int
> sundays = 
>   let centuryDays = drop (length (yeardays1 1900)) days in
>   length (filter goodSunday centuryDays)
>     where goodSunday (Sun, 1) = True
>           goodSunday _ = False

> main :: IO ()
> main = print ("There were " ++ show sundays ++ " such Sundays.")

~/save/versioned/projects/compiler/euler/19
$ time Euler
"There were 171 such Sundays."

real	0m0.007s
user	0m0.004s
sys	0m0.003s
