module Main where

fixed :: Eq a => [a] -> a 
fixed (x:[]) = x 
fixed (x:y:xs) = if x == y then y else fixed (y:xs) 

fuelCost :: Int -> Int 
fuelCost x | x > 8 = (quot x 3) - 2
           | otherwise = 0

fullFuel :: Int -> Int 
fullFuel f = fixed $ scanl (+) 0 $ iterate fuelCost (fuelCost f) 

--shipfuel :: [Int] -> Int 
--shipfuel xs = sum $ map additionalFuel $ map fuelCost xs



readMasses :: String -> IO [Int]
readMasses path = fmap (\x -> map read $ lines x) $ readFile path



main :: IO ()
main = do 
        masses <- readMasses "Day1/input.txt"
        print "fuel required for ship"
        print $ sum $ fmap fuelCost masses 
        print "full fuel for ship"
        print $ sum $ fmap fullFuel masses 



