module Main where 

import qualified Data.Text as T
import Data.List 
import Data.Map (Map)
import qualified Data.Map as Map 

size = (25,6)
skip = 25*6

processImage :: ([Int] -> a) -> Int -> [Int] -> [a]
processImage fnc n [] = []
processImage fnc n dat =  (fnc (take n dat)):processImage fnc n (drop n dat)


checklayers :: [Int] -> (Int,Int)
checklayers lyr = (length $ filter (==0) lyr, (length $ filter (==1) lyr)*(length $ filter (==2) lyr))


mergefunc :: Int -> Int -> Int 
mergefunc 0 _ = 0 
mergefunc 1 _ = 1 
mergefunc 2 x = x

scan :: (Int,Int) -> [(Int,Int)]
scan (w,0) = []
scan (w,h) = (fmap (\x -> (x,0)) [0..w-1])++rem where 
                                    rem = fmap (\(w,h) -> (w,h+1)) $ scan (w,h-1)

procfunc :: (Int,Int) -> [Int] -> Map (Int,Int) Int 
procfunc dim l = Map.fromList $ zip (scan dim) l


display :: (Int,Int) -> Map (Int,Int) Int -> String 
display (w,h) d = do
                    let swap = fmap (\x -> if x == 0 then ' ' else '*') d 
                    hp <- [0..h-1]
                    let withs = [0..(w-1)]
                    let v = fmap ((Map.!) swap) $ zip withs (repeat hp) :: [Char]
                    (v++"\n")

main = do 
            f <- readFile "Day8/input.txt"
            let s = fmap (\x -> read [x]) $ head .lines $ f :: [Int]
            let r = processImage checklayers (25*6) s
            print $ snd $ head $sortOn (fst) r 
            let maps = processImage (procfunc (25,6)) (25*6) s 
            let result = foldl (Map.unionWith mergefunc) Map.empty maps 
            putStr $ display (25,6) result 