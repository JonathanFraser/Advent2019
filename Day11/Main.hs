module Main where 

import IntCode
import Data.Map (Map)
import qualified Data.Map as Map 
import Control.Monad.Except

moveup (x,y) = (x,y+1)
movedown (x,y) = (x,y-1)
moveleft (x,y) = (x-1,y)
moveright (x,y) = (x+1,y) 

type Movement = ((Int,Int) -> (Int,Int))

movements :: [Movement]
movements = [moveup,moveright,movedown,moveleft]

turnleft :: [Movement] -> [Movement]
turnleft xs = (last xs):(init xs)

turnright :: [Movement] -> [Movement]
turnright xs = (tail xs)++[head xs]

type Robot = (Map (Int,Int) Int,(Int,Int),[Movement],Machine)

paint :: Robot -> Either String (Robot,StatusCode)
paint (sfc,loc,dir,m) = do 
                        let color = Map.findWithDefault 0 loc sfc :: Int 
                        (r,mn,outs) <- execute [color] m run
                        (nc,rot) <- case outs of 
                                     fst:snd:[] -> return (fst,snd)
                                     _ -> throwError "incorrect outputs"
                        let new_surface = Map.insert loc nc sfc 
                        let new_dir = (if rot == 0 then turnleft else turnright) dir 
                        let new_loc = (head new_dir) loc 
                        return ((new_surface,new_loc,new_dir,mn),r)


walkSurface :: Robot -> Either String (Map (Int,Int) Int) 
walkSurface r = do 
                 (n_r@(srf,_,_,_),s) <- paint r 
                 if s == Complete then 
                     return srf 
                 else 
                    walkSurface n_r

initRobot :: Machine -> Robot 
initRobot m = (Map.empty,(0,0),movements,m)

display :: Map (Int,Int) Char -> String 
display d = let 
                coords = Map.keys d
                x = fmap fst coords 
                y = fmap snd coords 
                xmin = minimum x 
                xmax = maximum x 
                ymin = minimum y 
                ymax = maximum y 
            in 
                do 
                    y <- reverse [ymin..ymax]
                    let row = do 
                               x <- [xmin..xmax]
                               return $ Map.findWithDefault ' ' (x,y) d
                    (row++"\n")



main = do 
        m <- readMachine "Day11/input.txt"
        let res = walkSurface $ initRobot m 
        case res of 
            Left x -> print x 
            Right y -> print $ Map.size y 

        let newRobot = (Map.singleton (0,0) 1,(0,0),movements,m)
        let res = walkSurface newRobot
        case res of 
            Left x -> print x 
            Right y -> putStrLn $ display $ fmap (\v -> if v == 1 then '#' else ' ') y

