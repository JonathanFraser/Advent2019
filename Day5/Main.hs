module Main where

import IntCode



main :: IO ()
main = do 
        prog <- readMachine "Day5/input.txt"
        let res = execute [1] prog (run) 
        print res 
