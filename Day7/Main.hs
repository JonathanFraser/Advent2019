module Main where 

import IntCode as I 
import Data.List 


runamps :: Machine -> (Int,Int,Int,Int,Int) -> Either String Int
runamps mach (a,b,c,d,e) = do 
                (j,_,r) <- execute [a,0] mach run 
                (k,_,s) <- execute (b:r) mach run 
                (l,_,t) <- execute (c:s) mach run 
                (m,_,w) <- execute (d:t) mach run 
                (n,_,x) <- execute (e:w) mach run
                return $ head x

initMachines :: Machine -> [Int] -> Either String [Machine]
initMachines m [] = Right []
initMachines m (x:xs) = let  
                        current = execute [x] m run 
                        remainer = initMachines m xs 
                        in 
                            do 
                                (cd,y,_) <- current
                                if cd == Complete then fail "machine is not waiting" else return () 
                                ys <- remainer 
                                return (y:ys)


stepMachines :: [Machine] -> [Int] -> Either String ([Machine],[Int])
stepMachines [] ins = Right ([],ins)
stepMachines (m:ms) ins = do 
                            (_,m1,ots) <- execute ins m run 
                            (ms,op) <- stepMachines ms ots 
                            return (m1:ms,op)

runloop :: [Machine] -> [Int] -> [Int] -> Either String [Int]
runloop m inputs prevout = do 
                    (sm,so) <- stepMachines m inputs 
                    case so of 
                        [] -> return prevout
                        _-> runloop sm so (prevout++so) 

tryinit :: Machine -> [Int] -> Either String [Int]
tryinit m set = do 
                 ms <- initMachines m set
                 runloop ms [0] []


tryallfb m = do 
            v <- permutations [5,6,7,8,9]
            return $ fmap last $ tryinit m v 

tryall m = do 
        (a:b:c:d:e:[]) <- permutations [0,1,2,3,4] 
        let setting = (a,b,c,d,e)
        return (runamps m setting)

test1 = fromString "3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5"

main = do 
        p <- readMachine "Day7/input.txt"
        print $ runamps p (0,1,2,3,4)
        print $ fmap maximum $ sequence $ tryall p 
        print $ fmap maximum $ sequence $ tryallfb p 
        
        --print $ runampsfb p (5,6,7,8,9)
