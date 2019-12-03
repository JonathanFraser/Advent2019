module Main where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set 
import qualified Data.Text as T
import Data.List

data Step = Step ((Int,Int) -> (Int,Int))

up (x,y) = (x,y+1)
down (x,y) = (x,y-1)
left (x,y) = (x-1,y)
right (x,y) = (x+1,y)

data Segments = Segments [(Step,Int)]

readStub :: String -> (Step,Int)
readStub (d:rest) = (v,l) where
                        v = case d of 
                            'U' -> Step up
                            'D' -> Step down
                            'L' -> Step left
                            'R' -> Step right
                        l = read rest 


readSegments :: String -> Segments
readSegments vals = Segments $ fmap (readStub . T.unpack) (T.split (==',') (T.pack vals)) 

readProblem :: String -> IO [Segments]
readProblem filepath = do 
                        f <- readFile filepath
                        return $ fmap readSegments $ lines f
                        

steps :: Segments -> [Step]
steps (Segments lst) = reverse $ foldl (\r (stp,n) -> (take n $ repeat stp)++r) [] lst


advance :: (Int,Int) -> Step -> (Int,Int) 
advance x (Step f) = f x 

walk :: (Int,Int) -> [Step] -> [(Int,Int)]
walk = scanl advance

pts :: Segments -> [(Int,Int)]
pts sg = walk (0,0) (steps sg)

nearest :: Segments -> Segments -> Maybe Int
nearest sg1 sg2 = case lst of 
                    x:[] ->  Nothing 
                    x:y:_ ->  Just y 
                    where 
                        lst = sort $ fmap (\(x,y) -> (abs x)+(abs y)) tscts
                        tscts = Set.toList $ Set.intersection s1 s2 
                        s1 = Set.fromList $ pts sg1 
                        s2 = Set.fromList $ pts sg2

shortest :: Segments -> Segments -> Maybe Int 
shortest sg1 sg2 = case dist of 
                    x:[] -> Nothing 
                    x:y:_ -> Just y 
                    where 
                        path1 = Map.fromList $ reverse $ zip (pts sg1) [0..]
                        path2 = Map.fromList $ reverse $ zip (pts sg2) [0..]
                        intersects = Map.intersectionWith (+) path1 path2 
                        dist = sort $ fmap snd $ Map.toList intersects 



main = do 
        prob <- readProblem "Day3/input.txt"
        let w1:w2:_ = prob
        print $ nearest w1 w2 
        print $ shortest w1 w2






        