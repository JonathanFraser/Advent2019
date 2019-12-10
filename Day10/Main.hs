module Main where 

import Data.Ratio
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad 
import Data.List 

toList :: String -> [(Int,Int)]
toList str = do 
            let k = lines str
            (h,l) <- zip [0..] k
            (w,c) <- zip [0..] l
            if c == '#' then return (h,w) else []

delta :: (Int,Int) -> (Int,Int) -> (Int,Int)
delta (h,w) (th,tw) = ((hp `div` r,wp `div` r),(hp*hp + wp*wp))
                                        where
                                            hp = h-th 
                                            wp = w-tw
                                            r = gcd hp wp

mix  :: [(Int,Int)] -> Map (Int,Int) (Set (Int,Int))
mix lst = fmap Set.fromList $ t where 
                                t = foldl (Map.unionWith (++)) Map.empty ret :: Map (Int,Int) [(Int,Int)]
                                ret = do 
                                        (h,w) <- lst 
                                        (th,tw) <- lst 
                                        guard ((th /= h) || (tw /= w))
                                        (r,d) = delta (h,w) (th,tw)
                                        return $ Map.singleton (h,w) [r]






 
t1  = "......#.#.\n#..#.#....\n..#######.\n.#.#.###..\n.#..#.....\n..#....#.#\n#..#....#.\n.##.#..###\n##...#..#.\n.#....####"

main = do 
        f <- readFile "Day10/input.txt" 
        let itms = toList t1
        let asteroids = toList f 
        let (pos,c) =  last $ sortOn snd $ Map.toList $ fmap Set.size $ mix $ asteroids
        print c 
        let remaining = delete pos asteroids 
        let deltas = fmap (\(th,tw)-> (h-th,w-tw)) remaining
        