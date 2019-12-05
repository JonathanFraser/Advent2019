module IntCode where

import Data.Array
import Control.Monad
import Control.Monad.State.Lazy
import qualified Data.Text as T
import Control.Monad.Except


data Machine = Machine {counter::Int, memory::Array Int Int} deriving Show
data ParameterMode = Position | Immediate deriving (Eq,Show)

type Operation a = StateT ([Int],Machine,[Int]) (Either String) a
type Modes = (ParameterMode,ParameterMode,ParameterMode)

incPC :: Operation ()
incPC = modify (\(i,m,o) -> (i,m {counter = 1 + counter m},o))

getPC :: Operation Int
getPC = do 
            (i,m,o) <- get 
            return $ counter m 

readValue :: ParameterMode -> Int -> Operation Int 
readValue Immediate loc = do 
                            (i,m,o) <- get
                            return (memory m ! loc)
readValue Position loc = do 
                          l <- readValue Immediate loc 
                          readValue Immediate l

readAdvance :: ParameterMode -> Operation Int 
readAdvance mode = do pc <- getPC;v <- readValue mode pc; incPC; return v 

readInput :: Operation Int 
readInput = do 
                (i,m,o) <- get
                case i of 
                    ip:is -> 
                        do put (is,m,o);return ip 
                    _ -> do pc <- getPC; throwError ("input unavailable "++(show pc))

writeOutput :: Int -> Operation () 
writeOutput v = do 
                (i,m,o) <- get 
                put (i,m,v:o)

writeValue :: ParameterMode -> Int -> Int -> Operation () 
writeValue Position loc val = do 
                            wl <- readValue Immediate loc
                            writeValue Immediate wl val 
writeValue Immediate loc val = do 
                                (i,m,o) <- get 
                                let mn = m {memory = memory m // [(loc,val)]}
                                put (i,mn,o)

writeAdvance :: ParameterMode -> Int -> Operation () 
writeAdvance mode val = do pc <- getPC;writeValue mode pc val;incPC 

p3 :: (Int -> Int -> Int) -> Bool -> Modes -> Operation Bool 
p3 fnc term (m1,m2,m3) = do 
                        v1 <- readAdvance m1 
                        v2 <- readAdvance m2
                        if (m3 == Position) then return () else throwError "write must be a positional mode" 
                        writeAdvance m3 (fnc v1 v2) 
                        return term 

add :: Modes -> Operation Bool 
add = p3 (+) False 

mul :: Modes -> Operation Bool 
mul = p3 (*) False 

inputOp :: ParameterMode -> Operation Bool
inputOp m = do 
                i <- readInput 
                writeAdvance m i 
                return False 

outputOp :: ParameterMode -> Operation Bool
outputOp m = do 
             v <- readAdvance m 
             writeOutput v 
             return False

decodeMode :: Int -> Operation ParameterMode 
decodeMode v = case v of 
                    0 ->
                        return Position
                    1 ->
                        return Immediate 
                    _ -> do 
                            pc <- getPC 
                            throwError ("unknown mode "++(show v)++" at pos "++(show pc))
                    
stripModes :: Int -> Operation (Modes,Int) 
stripModes v = do 
                let op = mod v 100 
                m1 <- decodeMode $ mod (quot v 100) 10
                m2 <- decodeMode $ mod (quot v 1000) 10
                m3 <- decodeMode $ mod (quot v 10000) 10
                return ((m1,m2,m3),op)

step :: Operation Bool 
step = do 
             intr <- readAdvance Immediate
             (m@(m1,m2,m3),op) <- stripModes intr 
             case op of
                1 -> 
                    add m 
                2 -> 
                    mul m 
                3 -> 
                    inputOp m1 
                4 -> 
                    outputOp m2
                99 ->
                    return True 
                r -> 
                    do 
                        pc <- getPC 
                        throwError ("unknown opcode "++(show r)++"at location"++(show (pc-1)))

run :: Operation () 
run = do 
        r <- step 
        if r then 
            return ()
        else 
            run 

execute :: [Int] -> Machine -> Operation a -> Either String (a,Machine,[Int])
execute inputs initial ops = do 
                                let sinit = (inputs,initial,[])
                                (result, sfinal) <- runStateT ops sinit
                                let (unusedInput,final,outputs) = sfinal
                                return (result,final,outputs)



result :: Operation Int 
result = readValue Immediate 0 


fromList  :: [Int] -> Machine 
fromList xs = Machine {
    counter = 0,
    memory = listArray (0,length xs-1) xs}

fromString :: String -> Machine 
fromString str = fromList (fmap (read . T.unpack) tkns) where
                    tkns = T.split (==',') (T.pack str)


readMachine :: String -> IO Machine
readMachine path = do 
                        f <- readFile path
                        return $ fromString f 


