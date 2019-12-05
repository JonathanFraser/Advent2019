module Main where

import Control.Monad
import IntCode


patch :: Operation () 
patch = setInput 12 2

setInput :: Int -> Int -> Operation ()
setInput noun verb = do 
                        writeValue Immediate 1 noun 
                        writeValue Immediate 2 verb 

scan :: Machine -> Int -> [(Int,Int)]
scan m tgt = do
            noun <- [0..100]
            verb <- [0..100]
            let res = execute [] m (setInput noun verb *> run *> result)
            guard $ case res of
                    Left _ -> False 
                    Right (v,_,_) -> v == tgt 
            return (noun,verb)


main :: IO ()
main = do 
        print $ execute [] (fromString "1,0,0,0,99") (run)
        prog <- readMachine "Day2/input.txt"
        let res = execute [] prog (patch *> run *> result) 
        print res --4714701
        let t = scan prog 19690720
        print t 
        print $ fmap (\(n,v) -> 100*n+v) t --5121


        --print prog