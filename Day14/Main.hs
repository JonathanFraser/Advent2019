{-# LANGUAGE FlexibleContexts #-}
module Main where 

import Data.Map (Map)
import qualified Data.Map as Map 
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Number
import Control.Monad 
import Data.List 
import Data.Maybe 

type Component = (String,Int) 
type Reaction = ([Component],Component)
type ReactionSet = [Reaction]


ident :: Stream s m Char => ParsecT s u m [Char]
ident = many (try upper) 

component :: Stream s m Char => ParsecT s u m Component
component = do 
             val <- int
             space 
             i <- ident
             return (i,val)

reaction :: Stream s m Char => ParsecT s u m Reaction 
reaction = do
            cmpnts <- (sepBy1 (try component) (try $ string ", "))
            spaces 
            string "=>"
            spaces 
            res <- component 
            spaces
            return $ (cmpnts,res)

parser :: Stream s m Char => ParsecT s u m ReactionSet 
parser = many reaction 

refCounts :: ReactionSet -> Map String Int
refCounts res = Map.fromList $ zip names counts where 
                                                namelist = group $ sort $ fmap fst $ concat $ fmap fst res
                                                names = fmap head namelist 
                                                counts = fmap length namelist 

dec :: Int -> Maybe Int 
dec 1 = Nothing 
dec n = Just (n-1)

run :: Map String Reaction -> Map String Int -> Map String Int -> Map String Int 
run rxns needs refs | Map.null refs = needs  
                    | Map.null (available_needs) = needs 
                    | otherwise = run rxns newneeds newrefs where 
                                                                available_needs = Map.difference needs refs
                                                                (name,amount) = head $ Map.toList available_needs
                                                                (inputs, (_,quantity)) = (Map.!) rxns name 
                                                                overflow = if (mod amount quantity) == 0 then 0 else 1 
                                                                rnxcount = (quot amount quantity) + overflow 
                                                                needsinc = Map.fromList $ fmap (\(n,v) -> (n,v*rnxcount)) inputs
                                                                newneeds = Map.unionWith (+) needsinc $ Map.delete name needs 
                                                                newrefs = (foldl (\f (n,_) -> f . (Map.update dec n)) id inputs) $ refs


computeOre :: ReactionSet -> Int -> Int 
computeOre rxn fuel = (Map.!) (run rxns needs refs) "ORE"where 
                                                refs = refCounts rxn 
                                                rxns = Map.fromList $ zip (fmap (fst.snd) rxn) rxn
                                                needs = Map.singleton "FUEL" fuel 

--bs :: (Int -> Int) -> Int ->  (Int,Int) -> Int 

main = do 
        f <- readFile "Day14/input.txt"
        let res = parse parser "" f
        case res of 
            Left s -> print s 
            Right p -> do print $ computeOre p 1 
        case res of 
            Left s -> print s 
            Right p -> print $ last $ takeWhile (\(f,o) ->o<=1000000000000)  $ do 
                                                                        f <- [4000000..4100000]
                                                                        return $ (f,computeOre p f)

        