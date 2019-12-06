module Main where 

import Data.Map (Map)
import qualified Data.Map as Map 
import Data.Tree (Tree)
import qualified Data.Tree as Tree

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'
split :: String -> [String]
split = wordsWhen (==')')

splitString :: String -> [(String,String)] 
splitString str = do 
                    l <- lines str
                    let x:y:[] = split l
                    return (x,y)


update :: Map String [String] -> (String,String) -> Map String [String]
update from (p,m) = Map.unionWith (++) n from where n = Map.singleton p [m]

collapse :: [(String,String)] -> Map String [String]
collapse = foldl update Map.empty

toTree :: Map String [String] -> Tree String 
toTree map = Tree.unfoldTree (\n -> (n,Map.findWithDefault [] n map)) "COM" 

fromString :: String -> Tree String 
fromString = toTree . collapse . splitString

readTree :: String -> IO (Tree String)
readTree path = do 
                        f <- readFile path
                        return $ fromString f 

test = unlines ["COM)B","B)C","C)D","D)E","E)F","B)G","G)H","D)I","E)J","J)K","K)L"]
test2 = unlines ["COM)B","B)C","C)D","D)E","E)F","B)G","G)H","D)I","E)J","J)K","K)L","K)YOU","I)SAN"]

orbitcount :: String -> [(Int,Int)] -> (Int,Int)
orbitcount _ [] = (1,0)
orbitcount _ xs = (1+sum cts, sum orbts + sum cts) where (cts,orbts) = unzip xs

transfercount :: String -> [(Int,Int,Int)] -> (Int,Int,Int)
transfercount "YOU" [] = (0,100000,100000) 
transfercount "SAN" [] = (100000,0,100000)
transfercount _ [] = (100000,100000,100000)
transfercount _ xs =  (r,s,min t (r+s-2)) where (r,s,t) = foldl (\(x,y,z) (a,b,c) -> (min x (a+1),min y (b+1), min z c)) (100000,100000,100000) xs

main = do 
        let testtree = fromString test
        print $ Tree.foldTree orbitcount testtree 
        print $ Tree.foldTree transfercount $ fromString test2
        t <- readTree "Day6/input.txt"
        print $ Tree.foldTree orbitcount t 
        print $ Tree.foldTree transfercount t 
       