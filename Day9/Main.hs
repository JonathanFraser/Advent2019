module Main where 

import IntCode 


extOutput x = do 
        (_,_,r) <- x
        return r 

main = do 
        let m1 = fromString "109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99"
        print $ extOutput $ execute [] m1 run 
        let m2 = fromString "1102,34915192,34915192,7,4,7,99,0"
        print $ extOutput $ execute [] m2 run 
        let m2 = fromString "104,1125899906842624,99"
        print $ extOutput $ execute [] m2 run 

        m <- readMachine "Day9/input.txt" 
        let x = do 
                (_,_,r) <- execute [1] m run
                return r 
        print x 
        let y = do 
               (_,_,x) <- execute [2] m run
               return x 
        print y