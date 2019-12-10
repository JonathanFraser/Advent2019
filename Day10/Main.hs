module Main where 

import Data.Ratio
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad 
import Data.List 
import Data.Maybe 

toList :: String -> [(Int,Int)]
toList str = do 
            let k = lines str
            (h,l) <- zip [0..] k
            (w,c) <- zip [0..] l
            if c == '#' then return (h,w) else []

delta :: (Int,Int) -> (Int,Int) -> ((Int,Int),Int)
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
                                        let (r,d) = delta (h,w) (th,tw)
                                        return $ Map.singleton (h,w) [r]






 
t1  = "......#.#.\n#..#.#....\n..#######.\n.#.#.###..\n.#..#.....\n..#....#.#\n#..#....#.\n.##.#..###\n##...#..#.\n.#....####"
tl = ".#..##.###...#######\n##.############..##.\n.#.######.########.#\n.###.#######.####.#.\n#####.##.#.##.###.##\n..#####..#.#########\n####################\n#.####....###.#.#.##\n##.#################\n#####.##.###..####..\n..######..##.#######\n####.##.####...##..#\n.#####..#.######.###\n##...#.##########...\n#.##########.#######\n.####.#.###.###.#.##\n....##.##.###..#####\n.#.#.###########.###\n#.#.#.#####.####.###\n###.##.####.##.#..##"

main = do 
        f <- readFile "Day10/input.txt" 
        --let f = tl 
        let itms = toList t1
        let asteroids = toList f 
        print $ maximum $ fmap fst asteroids 
        print $ maximum $ fmap snd asteroids 
        let (pos,c) =  last $ sortOn snd $ Map.toList $ fmap Set.size $ mix $ asteroids
        print pos 
        print c 
        let remaining = delete pos asteroids 
        let deltas = fmap (delta pos) remaining
        let lookup = Map.fromList $ zip deltas remaining
        let distances = sort $ nub $ fmap snd deltas
        let angles = reverse $ sortOn (\(h,w) -> atan2 (fromIntegral (-1*w)) (fromIntegral (-1*h))) $ nub $ fmap fst deltas 
        let coords = fmap catMaybes $ do  
                                        a <- angles 
                                        return $ fmap (\x -> Map.lookup (a,x) lookup) distances 

        let crds = concatMap id $ transpose coords 
        print $ take 10 crds 
        
        --let deltas = fmap (\(th,tw)-> (h-th,w-tw)) remaining
        let val@(hv,wv) = head $ drop 199 crds 
        print val 
        print (wv*100+hv)
        