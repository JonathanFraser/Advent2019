module Main where 

import Linear.V3 
import Linear.Vector
import Data.Set (Set)
import qualified Data.Set as Set 
import Control.Lens

type Vector = V3 Int 


energy :: Vector -> Int 
energy v = foldl (+) 0 $ fmap abs v

pos =  [V3 1 (-4) 3,V3 (-14) 9 (-4),V3 (-4) (-6) 7,V3 6 (-9) (-11)]
vel =  repeat $ V3 0 0 0 

vdelta :: Vector -> Vector -> Vector 
vdelta m1 m2 = fmap signum (m2 - m1)


step :: [(Vector,Vector)] -> [(Vector,Vector)]
step states = do 
                (p,v) <- states 
                let vdeltas = fmap (vdelta p . fst) states
                let vdelta = foldl (^+^) (V3 0 0 0) vdeltas 
                return (p+v+vdelta,v+vdelta)

totalenergy :: (Vector,Vector) -> Int 
totalenergy (v1,v2) = (energy v1)*(energy v2)

systemenergy :: [(Vector,Vector)] -> Int 
systemenergy y  = sum $ fmap totalenergy y

progress :: [[(Vector,Vector)]]
progress = iterate step $ zip pos vel 

computecycle  :: Eq a => Int -> a -> [a] -> Int 
computecycle v tgt [] = undefined 
computecycle v tgt (x:xs) = if x == tgt then  v+1 else computecycle (v+1) tgt xs 

dir :: (Vector -> Int) -> (Vector,Vector) -> (Int,Int) 
dir l (s1,s2) = (l s1, l s2)

ext :: (a -> b) -> [[a]] -> [[b]]
ext f = fmap (fmap f) 

periodic :: (Vector -> Int) -> [[(Vector,Vector)]] -> Int 
periodic f lst = computecycle 0 s rest where
                                vals = ext (dir f) lst 
                                s = head vals 
                                rest = tail vals 
main = do 
        let v = head $ drop 1000 progress 
        print $ systemenergy v 
        let xperiod = periodic (view _x) progress
        let yperiod = periodic (view _y) progress 
        let zperiod = periodic (view _z) progress
        let g = gcd (gcd xperiod yperiod) zperiod
        print $ (quot xperiod g) *(quot yperiod g) *(quot zperiod g)
