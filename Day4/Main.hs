module Main where

import Control.Monad


tonumber (a,b,c,d,e,f) = f + 10*e + 100*d + 1000*c + 10000*b + 100000*a

part1 :: [(Int,Int,Int,Int,Int,Int)]
part1 = do 
            a <- [0..9]
            b <- [0..9]
            c <- [0..9]
            d <- [0..9]
            e <- [0..9]
            f <- [0..9]

            guard ((a == b) || (b == c) || (c == d) || (d == e) || (e == f))
            guard ((b >= a) && (c >= b) && (d >= c) && (e >= d) && (f >= e))

            let n = tonumber (a,b,c,d,e,f)
            guard (n >= 307237)
            guard (n <= 769058)     

            return (a,b,c,d,e,f)


part2 = do 
            (a,b,c,d,e,f) <- part1 
            guard (((a == b)&&(b/=c)) || ((a /= b)&&(b == c)&&(c /= d)) || ((b /= c)&&(c == d)&&(d/=e)) || ((c/=d)&&(d == e)&&(e /= f)) || ((d/=e)&&(e == f)))
            return (a,b,c,d,e,f)

main = do 
        print part1
        print $ length $ part1
        print $ length part2