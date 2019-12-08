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
scan (w,h) = do 
                a <- [0..h-1]
                b <- [0..w-1]
                return (b,a)


procfunc :: (Int,Int) -> [Int] -> Map (Int,Int) Int 
procfunc dim l = Map.fromList $ zip (scan dim) l


display ::(Int,Int) -> Map (Int,Int) Char -> String 
display (w,h) d = do
                    hp <- [0..h-1]
                    let v = do 
                                wp <- [0..(w-1)]
                                return $ (Map.!) d (wp,hp)
                    (v++"\n")


main = do 
            f <- readFile "Day8/input.txt"
            let s = fmap (\x -> read [x]) $ head .lines $ f :: [Int]
            let r = processImage checklayers (25*6) s
            print $ snd $ head $sortOn (fst) r 
            let maps = processImage (procfunc (25,6)) (25*6) s 
            let result = foldl (Map.unionWith mergefunc) Map.empty maps 
            let pixels = fmap (\x -> if x == 0 then ' ' else '*') result
            putStr $ display (25,6) pixels