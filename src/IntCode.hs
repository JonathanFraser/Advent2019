module IntCode where

import Data.Array
import Control.Monad
import qualified Data.Text as T


data Machine = Machine {counter::Int, memory::Array Int Int} deriving Show

fromList  :: [Int] -> Machine 
fromList xs = Machine {
    counter = 0,
    memory = listArray (0,length xs-1) xs}


result :: [Machine] -> Int 
result (p:_) = (memory p) ! 0

advance :: [Machine] -> ([Machine],Either String Bool)
advance m@(x:_) = case opcode of 
                1 -> 
                    (x {counter = pc+4, memory = memory x // [(resloc,op1+op2)]}:m,Right True)
                2 -> 
                    (x {counter = pc+4, memory = memory x // [(resloc,op1*op2)]}:m,Right True)
                99 ->
                    (m,Right False)
                r -> 
                    (m,Left ("unknown opcode "++(show r)++"at location"++(show pc)))
                where 
                    pc = counter x
                    opcode = memory x ! pc
                    op1loc = memory x ! (pc+1)
                    op2loc = memory x ! (pc+2)
                    op1 = memory x ! op1loc
                    op2 = memory x ! op2loc
                    resloc = memory x ! (pc+3)

runMachine :: [Machine] -> ([Machine],Either String Int)
runMachine m =  case step of 
                    Left s -> 
                        (ms,Left s) 
                    Right False -> 
                        (ms,Right (result ms))
                    Right True ->
                        runMachine ms
                where 
                    (ms,step) = advance m

readMachine :: String -> IO Machine
readMachine path = do 
                        f <- readFile path
                        let tkns = T.split (==',') (T.pack f)
                        return $ fromList (fmap (read . T.unpack) tkns) 


examples = [
    fromList [1,9,10,3,2,3,11,0,99,30,40,50],
    fromList [1,0,0,0,99],
    fromList [2,3,0,3,99],
    fromList [2,4,4,5,99,0],
    fromList [1,1,1,4,99,5,6,0,99]]


patch :: Machine -> Machine 
patch = setInput 12 2

setInput :: Int -> Int -> Machine -> Machine 
setInput noun verb m = m {memory = memory m // [(1,noun),(2,verb)]}


scan :: Machine -> Int -> [(Int,Int)]
scan m tgt = do
            noun <- [0..100]
            verb <- [0..100]
            let (_,res) = runMachine [setInput noun verb m]
            guard $ case res of
                    Left _ -> False 
                    Right v -> v == tgt 
            return (noun,verb)
