module IntCode where

import Data.Map (Map)
import Data.Maybe
import qualified Data.Map as Map 
import Control.Monad
import Control.Monad.State.Lazy
import qualified Data.Text as T
import Control.Monad.Except


data Machine = Machine {counter::Int, relbase::Int, memory::Map Int Int} deriving Show
data ParameterMode = Position | Immediate | Relative deriving (Eq,Show)
data StatusCode = Running | Waiting | Complete deriving (Eq,Show)

type Operation a = StateT ([Int],Machine,[Int]) (Either String)  a
type Modes = (ParameterMode,ParameterMode,ParameterMode)

incPC :: Operation ()
incPC = modify (\(i,m,o) -> (i,m {counter = 1 + counter m},o))

decPC :: Operation ()
decPC = modify (\(i,m,o) -> (i,m {counter = -1 + counter m},o))

getPC :: Operation Int
getPC = do 
            (i,m,o) <- get 
            return $ counter m 

adjRelbase :: Int -> Operation () 
adjRelbase x = modify (\(i,m,o) -> (i,m {relbase = x + relbase m},o))

getRelBase :: Operation Int
getRelBase = do 
            (_,m,_) <- get
            return $ relbase m 

setPC :: Int -> Operation () 
setPC v = modify (\(i,m,o) -> (i,m {counter = v},o))

readValue :: ParameterMode -> Int -> Operation Int 
readValue Immediate loc = do 
                            (i,m,o) <- get
                            let res = Map.lookup loc (memory m) :: Maybe Int
                            case res of 
                                    Nothing -> return 0
                                    Just x -> return x 
readValue Position loc = do 
                          l <- readValue Immediate loc 
                          readValue Immediate l
readValue Relative loc = do 
                            l <- readValue Immediate loc
                            b <- getRelBase  
                            readValue Immediate (l+b)

readAdvance :: ParameterMode -> Operation Int 
readAdvance mode = do pc <- getPC;v <- readValue mode pc; incPC; return v 

readInput :: Operation (Maybe Int) 
readInput = do 
                (i,m,o) <- get
                case i of 
                    ip:is -> 
                        do put (is,m,o);return $ Just ip 
                    [] -> return Nothing

writeOutput :: Int -> Operation () 
writeOutput v = do 
                (i,m,o) <- get 
                put (i,m,v:o)

writeValue :: ParameterMode -> Int -> Int -> Operation () 
writeValue Position loc val = do 
                            wl <- readValue Immediate loc
                            writeValue Immediate wl val 
writeValue Relative loc val = do 
                                wl <- readValue Immediate loc
                                b <- getRelBase
                                writeValue Immediate (wl+b) val 
writeValue Immediate loc val = do 
                                (i,m,o) <- get 
                                let mn = m {memory = Map.insert loc val $ memory m}
                                put (i,mn,o)

writeAdvance :: ParameterMode -> Int -> Operation () 
writeAdvance mode val = do pc <- getPC;writeValue mode pc val;incPC 

p3 :: (Int -> Int -> Int) -> StatusCode -> Modes -> Operation StatusCode 
p3 fnc term (m1,m2,m3) = do 
                        v1 <- readAdvance m1 
                        v2 <- readAdvance m2
                        if (m3 /= Immediate) then return () else throwError "write must not be Immediate mode" 
                        writeAdvance m3 (fnc v1 v2) 
                        return term 

add :: Modes -> Operation StatusCode 
add = p3 (+) Running

mul :: Modes -> Operation StatusCode 
mul = p3 (*) Running 

lt :: Modes -> Operation StatusCode 
lt = p3 (\x y -> if x < y then 1 else 0) Running

eq :: Modes -> Operation StatusCode 
eq = p3 (\x y -> if x == y then 1 else 0) Running

inputOp :: ParameterMode -> Operation StatusCode
inputOp m = do 
                i <- readInput 
                case i of 
                    Just d -> do writeAdvance m d;return Running
                    Nothing -> do decPC;return Waiting

outputOp :: ParameterMode -> Operation StatusCode
outputOp m = do 
             v <- readAdvance m 
             writeOutput v 
             return Running

decodeMode :: Int -> Operation ParameterMode 
decodeMode v = case v of 
                    0 ->
                        return Position
                    1 ->
                        return Immediate 
                    2 -> 
                        return Relative
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

step :: Operation StatusCode 
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
                    outputOp m1
                5 -> 
                    do 
                        p1 <- readAdvance m1
                        p2 <- readAdvance m2 
                        if p1 /= 0 then setPC p2 else return () 
                        return Running 
                6 ->
                    do 
                        p1 <- readAdvance m1
                        p2 <- readAdvance m2 
                        if p1 == 0 then setPC p2 else return () 
                        return Running 
                7 ->
                    lt m
                8 -> 
                    eq m 
                9 -> 
                    do 
                        p1 <- readAdvance m1
                        adjRelbase p1
                        return Running

                99 ->
                    do decPC;return Complete 
                r -> 
                    do 
                        pc <- getPC 
                        throwError ("unknown opcode "++(show r)++"at location"++(show (pc-1)))

run :: Operation StatusCode
run = do 
        r <- step
        case r of 
            Complete ->  return Complete
            Waiting -> return Waiting 
            Running -> run 

execute :: [Int] -> Machine -> Operation a -> Either String (a,Machine,[Int])
execute inputs initial ops = do 
                                let sinit = (inputs,initial,[])
                                (result, sfinal) <- runStateT ops sinit
                                let (unusedInput,final,outputs) = sfinal
                                return (result,final,reverse outputs)


result :: Operation Int 
result = readValue Immediate 0 


fromList  :: [Int] -> Machine 
fromList xs = Machine {
    counter = 0,
    relbase = 0,
    memory = Map.fromList $ zip [0..] xs}

fromString :: String -> Machine 
fromString str = fromList (fmap (read . T.unpack) tkns) where
                    tkns = T.split (==',') (T.pack str)


readMachine :: String -> IO Machine
readMachine path = do 
                        f <- readFile path
                        return $ fromString f 


