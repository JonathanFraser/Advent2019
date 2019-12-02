module Main where

import Data.Map as Map 

data Program = Program {counter::Int, memory::Map.Map Int Int}

result :: Program -> Int 
result = undefined 

advance :: Program -> Program 
advance = id

main :: IO ()
main = print "" 