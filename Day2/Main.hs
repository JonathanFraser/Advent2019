module Main where

import Data.Array
import Control.Monad
import qualified Data.Text as T
import IntCode

data Machine = Machine {counter::Int, memory::Array Int Int} deriving Show


main :: IO ()
main = do 
        prog <- readMachine "Day2/input.txt"
        let n = patch prog 
        let (history,result) = runMachine [n]
        void $ sequence $ fmap print history 
        print result --4714701
        let t = scan prog 19690720
        print t 
        print $ fmap (\(n,v) -> 100*n+v) t --5121


        --print prog