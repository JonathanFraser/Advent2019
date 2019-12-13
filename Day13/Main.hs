module Main where 

import IntCode 
import Data.Map (Map)
import qualified Data.Map as Map 

makescreen :: [Int]  -> Map (Int,Int) Int -> Map (Int,Int) Int 
makescreen [] m = m
makescreen (x:y:v:r) m = makescreen r (Map.insert (x,y) v m)

display :: Map (Int,Int) Char -> String 
display d = let 
                coords = Map.keys d
                x = fmap fst coords 
                y = fmap snd coords 
                xmin = minimum x 
                xmax = maximum x 
                ymin = minimum y 
                ymax = maximum y 
            in 
                do 
                    y <- reverse [ymin..ymax]
                    let row = do 
                               x <- [xmin..xmax]
                               return $ Map.findWithDefault ' ' (x,y) d
                    (row++"\n")

conv ::  Int -> Char 
conv 0 = ' '
conv 1 = '#'
conv 2 = '%'
conv 3 = '='
conv 4 = 'O'

extractscore :: Map (Int,Int) Int -> (Map (Int,Int) Int,Int)
extractscore d = (n,s) where 
                        s = Map.findWithDefault 0 (-1,0) d
                        n = Map.delete (-1,0) d 

controlinput :: Map (Int,Int) Int -> Int 
controlinput scrn = signum (bx - px)
                        where 
                            (bx,by) = head $ Map.keys $ Map.filter (==4) scrn
                            (px,py) = head $ Map.keys $ Map.filter (==3) scrn 

fmtscreen :: Map (Int,Int) Int -> String 
fmtscreen m = ("score: "++(show s)++"\n")++(display $ fmap conv rem) where (rem,s) = extractscore m

play :: Machine -> Map (Int,Int) Int -> Either String (Map (Int,Int) Int)
play m state = do 
            let inp = controlinput state 
            (r,mn,ots) <- execute [inp] m run 
            let newstate = makescreen ots state
            case r of
                Complete -> return newstate
                Waiting -> play mn newstate 

playgame :: Machine -> Either String String 
playgame m = do 
                (v,mn,ots) <- execute [] m (writeValue Immediate 0 2 *> run)
                let state = makescreen ots Map.empty
                d <- play mn state 
                return $ fmtscreen d 

main = do 
        m <- readMachine "Day13/input.txt"
        let res = execute [] m run 
        case res of 
            Left s -> print s 
            Right (a,m,ots) -> do 
                                let scrn = makescreen ots Map.empty
                                putStr $ fmtscreen scrn
                                print $ Map.size $ Map.filter (==2) scrn
                                print $ a
        let res2 = playgame m 
        case res2 of 
            Left s -> print s 
            Right t -> putStr t 